{-# Language TemplateHaskell #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language DeriveDataTypeable #-}
module HSync.Server.FileSystemState( FSState(..)
                                   , newFSState


                                   , FileLabel(..)
                                   , fromNotification
                                   , toNotification

                                   , updateNotification
                                   , getNotificationsAsOf
                                   , Directory'
                                   , ReversedSubPath

                                   , withTimedFSTree
                                   ) where

import Prelude

import Control.Applicative((<$>))
import Control.Monad.IO.Class(MonadIO(..))


import Data.Aeson.TH
import Data.Data(Data, Typeable)
import Data.Monoid
import Data.Sequence(Seq)



import Data.SafeCopy(base, deriveSafeCopy)

import HSync.Common.TimedFSTree
import HSync.Common.FSTree
import HSync.Common.DateTime
import HSync.Common.FileIdent(FileIdent, fileIdent, getDateTime)
import HSync.Common.Notification( Event(..)
                                , EventKind(..)
                                , Notification(Notification)
                                , event
                                )
import HSync.Common.Types

import qualified Data.Foldable             as F
import qualified Data.Sequence             as S
import qualified HSync.Common.Notification as N

--------------------------------------------------------------------------------

-- | The data that we store for each file. This should allow us to reconstruct
-- a notification once we have the path.
data FileLabel = FileLabel { eventKind    :: EventKind
                           , changee      :: ClientIdent
                           , timestamp    :: DateTime
                           , oldFileIdent :: FileIdent
                           }
                 deriving (Show,Eq,Data,Typeable)

$(deriveJSON defaultOptions ''FileLabel)
$(deriveSafeCopy 0 'base ''FileLabel)

instance AsDateTime FileLabel where
  toDateTime = timestamp


-- | Extract a file label from a Notification
fromNotification                                     :: Notification -> FileLabel
fromNotification (Notification (Event ek _ fi) ci t) = FileLabel ek ci t fi


-- | Given a file label and a path. Reconstruct the corresponding notification
toNotification                          :: FileLabel -> Path -> Notification
toNotification (FileLabel ek ci t fi) p = Notification (Event ek p fi) ci t

--------------------------------------------------------------------------------

newtype FSState = FSState { fsStateTree :: TimedFSTree FileLabel }
                  deriving (Show,Eq,Data,Typeable)

$(deriveJSON defaultOptions ''FSState)
$(deriveSafeCopy 0 'base ''FSState)

-- | Read a new instance of the FSState from disk. All notifications will be of
-- the kind 'fileAdded', and have the file modification time as the associated
-- datetime.
newFSState         :: (MonadIO m, Functor m) => FilePath -> m FSState
newFSState baseDir = readDirectory baseDir f >>=
                       maybe (error "newFSState") -- TODO: fix
                             (return . FSState . FSTree)
  where
    f fp    = (\fi -> let d = getDateTime fi in
                      FileData (mkLabel d fi) d) <$> fileIdent fp
    mkLabel = FileLabel FileAdded (ClientIdent "unknown")


withTimedFSTree   :: (TimedFSTree FileLabel -> TimedFSTree FileLabel)
                  -> FSState -> FSState
withTimedFSTree f = FSState . f . fsStateTree

--------------------------------------------------------------------------------

-- | Update the FileSystem State by the incoming notification. I.e. update,
-- add, or delete the indicated item in the tree.
updateNotification   :: Notification -> FSState -> FSState
updateNotification n = withTimedFSTree (FSTree . f . unTree)
  where
    f = case kind . event $ n of
         FileAdded        -> safe addFileAt newFile
         FileRemoved      -> updateFileAt fullP gf
         FileUpdated      -> updateFileAt fullP gf
         DirectoryAdded   -> safe addDirectoryAt newDir
         DirectoryRemoved -> updateDirectoryAt fullP gd

    gf file' = file' { fileData = fData }
    gd dir   = dir   { dirData        = fData
                     , files          = mempty  -- To Save space
                     , subDirectories = mempty  -- To save space
                     }


    fData   = FileData (fromNotification n) dt
    dt      = N.timestamp n

    newFile = File fName fData (measure fData)
    newDir  = emptyDirectory fName fData


    p'          = affectedPath . event $ n
    (p'',fName) = andLast $ subPath p'

    fullP = (unUI $ owner p') : subPath p'

    -- If we add a file or directory, make sure to remove any earlier files or
    -- sub-directory that was in the tree before
    safe        :: IsFileOrDirectory t
                => ( SubPath -> t (Max DateTime) (FileData FileLabel)
                    -> Directory' -> Directory'
                   )   -- ^ addF
                -> t (Max DateTime) (FileData FileLabel)
                -> Directory' -> Directory'
    safe addF x = addF              p'' x
                . deleteFileAt      p'' fName (Max dt)
                . deleteDirectoryAt p'' fName (Max dt)


type Directory' = Directory (Max DateTime) (FileData FileLabel)

type ReversedSubPath = SubPath


-- TODO: Check if this now all works for deleted files


-- | Get all notifications for files stored in the filesystem rooted at this
-- directory, in increasing order of time at which they happened.
-- The clientId are the userId to use in the path, and ReverseSubPath the subpath
-- pointing to the file, stored in reverse order.
getNotificationsAsOf          :: DateTime -> UserIdent -> ReversedSubPath
                              -> Directory' -> [Notification]
getNotificationsAsOf dt u rp dir
  | measurement dir < Max dt  = [] -- All changes in this directory occured before dt.
  | otherwise                 = myChanges `merge` fileChanges `merge` recursiveChanges

    where
      -- localDirChanges  = changes . subDirectories $ dir
      myChanges        = changes . S.singleton $ dir
      fileChanges      = changes . files       $ dir
      recursiveChanges = foldr1 merge [ getNotificationsAsOf dt u (name d : rp) d
                                      | d <- F.toList $ subDirectories dir
                                      ]

      changes :: IsFileOrDirectory t
              => Seq (t m (FileData FileLabel))
              -> [Notification]
      changes = F.toList
              . S.unstableSort
              . fmap toNotification'
              . S.filter (\x -> timestamp' x >= dt)

      timestamp' :: IsFileOrDirectory t => t m (FileData FileLabel) -> DateTime
      timestamp' = timestamp . extractData . dataValue


      toNotification'   :: IsFileOrDirectory t => t m (FileData FileLabel) -> Notification
      toNotification' x = toNotification (extractData $ dataValue x) (rp </> name x)

      ps </> n = Path u $ reverse (n : ps)


-- | Merge two sorted lists into one sorted list
merge               :: Ord a => [a] -> [a] -> [a]
merge []     ys     = ys
merge xs     []     = xs
merge (x:xs) (y:ys)
  | x <= y          = x : merge xs       (y : ys)
  | otherwise       = y : merge (x : xs) ys
