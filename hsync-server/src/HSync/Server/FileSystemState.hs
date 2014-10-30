{-# Language TemplateHaskell #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language DeriveDataTypeable #-}
module HSync.Server.FileSystemState( FSState(..)
                                   , newFSState
                                   , newUserFSState

                                   , addUserToFSState

                                   , FileLabel(..)
                                   , fromNotification
                                   , toNotification

                                   , updateNotification
                                   , getNotificationsAsOf
                                   , File', Directory'
                                   , ReversedSubPath

                                   , withTimedFSTree
                                   ) where

import Prelude


import Control.Applicative((<$>))
import Control.Monad((<=<))
import Control.Monad.IO.Class(MonadIO(..))

import Data.Aeson.TH
import Data.Data(Data, Typeable)
import Data.Monoid
import Data.Sequence(Seq)

import Data.Map(Map)


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
import qualified Data.Map                  as M
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

newtype FSState = FSState { unM :: Map UserIdent (TimedFSTree FileLabel) }
                deriving (Show,Eq,Data,Typeable)


$(deriveJSON defaultOptions ''Map)


$(deriveJSON defaultOptions ''FSState)
$(deriveSafeCopy 0 'base ''FSState)

-- | Construct a new FSState from the disk. The file path is the path to
-- a directory s.t. each directory corresponds to the FSTree of that user
--
  -- All file labels will be of the kind 'fileAdded', and have the file
  -- modification time as the associated datetime.
newFSState         :: (MonadIO m, Functor m) => FilePath -> m FSState
newFSState baseDir = f <$> newUserFSState baseDir
  where
    f (FSTree dir) = FSState $ M.fromList [ (userIdent' . name $ d,  FSTree d)
                                          | d <- F.toList . subDirectories $ dir
                                          ]

    userIdent' = either (error "newFSState': userIdent not valid") id . userIdent

-- | Build the tree for one user
newUserFSState     :: (MonadIO m, Functor m) => FilePath -> m (TimedFSTree FileLabel)
newUserFSState dir = maybe (error "newFSState'") FSTree <$> readDirectory dir f
  where
    f fp    = (\fi -> let d = getDateTime fi in
                      FileData (mkLabel d fi) d) <$> fileIdent fp
    mkLabel = FileLabel FileAdded (ClientIdent "unknown")



-- | Given a user and a Tree. Add this user/tree combination to the FSState.
addUserToFSState                 :: UserIdent
                                 -> TimedFSTree FileLabel
                                 -> FSState -> FSState
addUserToFSState u t (FSState m) = FSState $ M.insert u t m


-- | Given a userident, and a function to maniuplate the FSTree. Update the
--   tree of the selected user.
withTimedFSTree     :: UserIdent
                    -> (TimedFSTree FileLabel -> TimedFSTree FileLabel)
                    -> FSState -> FSState
withTimedFSTree u f = FSState . M.adjust f u . unM

--------------------------------------------------------------------------------

-- | Update the FileSystem State by the incoming notification. I.e. update,
-- add, or delete the indicated item in the tree.
updateNotification   :: Notification -> FSState -> FSState
updateNotification n = withTimedFSTree (owner p) (FSTree . f . unTree)
  where
    f = case kind . event $ n of
         FileAdded        -> safe addFileAt newFile
         FileRemoved      -> updateFileAt sp gf
         FileUpdated      -> updateFileAt sp gf
         DirectoryAdded   -> safe addDirectoryAt newDir
         DirectoryRemoved -> updateDirectoryAt sp gd

    gf file' = file' { fileData = fData }
    gd dir   = dir   { dirData        = fData
                     , files          = mempty  -- To Save space
                     , subDirectories = mempty  -- To save space
                     }


    fData   = FileData (fromNotification n) dt
    dt      = N.timestamp n

    newFile = File fName fData (measure fData)
    newDir  = emptyDirectory fName fData


    p           = affectedPath . event $ n
    sp          = subPath p
    (sp',fName) = andLast sp



    -- If we add a file or directory, make sure to remove any earlier files or
    -- sub-directory that was in the tree before
    safe        :: IsFileOrDirectory t
                => ( SubPath -> t (Max DateTime) (FileData FileLabel)
                    -> Directory' -> Directory'
                   )   -- ^ addF
                -> t (Max DateTime) (FileData FileLabel)
                -> Directory' -> Directory'
    safe addF x = addF              sp' x
                . deleteFileAt      sp' fName (Max dt)
                . deleteDirectoryAt sp' fName (Max dt)


type File'      = File      (Max DateTime) (FileData FileLabel)
type Directory' = Directory (Max DateTime) (FileData FileLabel)

type ReversedSubPath = SubPath


-- TODO: Check if this now all works for deleted files


-- | Get all notifications for files stored in the filesystem rooted at path
-- directory, in increasing order of time at which they happened.
getNotificationsAsOf      :: DateTime -> Path -> FSState -> [Notification]
getNotificationsAsOf dt p = maybe [] getNots . getDir . unM
  where
    getNots = getNotificationsAsOf' dt (owner p) (reverse . subPath $ p)
    getDir  = findDirectoryAt (subPath p) . unTree <=< M.lookup (owner p)


-- | Get all notifications for files stored in the filesystem rooted at this
-- directory, in increasing order of time at which they happened.
-- The clientId are the userId to use in the path, and ReverseSubPath the subpath
-- pointing to this directory, stored in reverse order.
getNotificationsAsOf'          :: DateTime -> UserIdent -> ReversedSubPath
                               -> Directory' -> [Notification]
getNotificationsAsOf' dt u rp dir
  | measurement dir < Max dt  = [] -- All changes in this directory occured before dt.
  | otherwise                 = myChanges `merge` fileChanges `merge` recursiveChanges

    where
      -- localDirChanges  = changes . subDirectories $ dir
      fileChanges      = changes . files       $ dir
      recursiveChanges = foldr merge [] [ getNotificationsAsOf' dt u (name d : rp) d
                                        | d <- F.toList $ subDirectories dir
                                        ]
      myChanges
        | timestamp' dir > dt = [toNotification'' dir $ Path u (reverse rp)]
        | otherwise           = []


      changes :: IsFileOrDirectory t
              => Seq (t m (FileData FileLabel))
              -> [Notification]
      changes = F.toList
              . S.unstableSort
              . fmap toNotification'
              . S.filter (\x -> timestamp' x > dt)

      timestamp' :: IsFileOrDirectory t => t m (FileData FileLabel) -> DateTime
      timestamp' = timestamp . extractData . dataValue

      toNotification'   :: IsFileOrDirectory t => t m (FileData FileLabel) -> Notification
      toNotification' x =  toNotification'' x (rp </> name x)

      toNotification''   :: IsFileOrDirectory t => t m (FileData FileLabel)
                         -> Path -> Notification
      toNotification'' x = toNotification (extractData $ dataValue x)

      ps </> n = Path u $ reverse (n : ps)


-- | Merge two sorted lists into one sorted list
merge               :: Ord a => [a] -> [a] -> [a]
merge []     ys     = ys
merge xs     []     = xs
merge (x:xs) (y:ys)
  | x <= y          = x : merge xs       (y : ys)
  | otherwise       = y : merge (x : xs) ys
