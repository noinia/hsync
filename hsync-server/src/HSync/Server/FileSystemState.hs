{-# Language TemplateHaskell #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language DeriveDataTypeable #-}
module HSync.Server.FileSystemState( FSState(..)
                                   , newFSState


                                   , FileLabel(..)
                                   , fromNotification

                                   , updateNotification

                                   , withTimedFSTree
                                   ) where

import Prelude

import Control.Applicative((<$>))
import Control.Monad.IO.Class(MonadIO(..))

import Data.Aeson.TH
import Data.Data(Data, Typeable)

import Data.SafeCopy(base, deriveSafeCopy)

import HSync.Common.TimedFSTree
import HSync.Common.FSTree
import HSync.Common.DateTime
import HSync.Common.FileIdent(FileIdent)
import HSync.Common.Notification( Event(..)
                                , involvesFile
                                , EventKind(..)
                                , Notification(Notification)
                                , event
                                )
import HSync.Common.Types

import qualified HSync.Common.Notification as N

--------------------------------------------------------------------------------

data FileLabel = FileLabel { eventKind :: EventKind
                           , changee   :: ClientIdent
                           , timestamp :: DateTime
                           , fileIdent :: Maybe FileIdent
                           }
                 deriving (Show,Eq,Data,Typeable)

$(deriveJSON defaultOptions ''FileLabel)
$(deriveSafeCopy 0 'base ''FileLabel)

instance AsDateTime FileLabel where
  toDateTime = timestamp



fromNotification                                     :: Notification -> FileLabel
fromNotification (Notification (Event ek _ fi) ci t) = FileLabel ek ci t (Just fi)

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
    f fp       = (\d -> FileData (mkLabel d) d) <$> (modificationTime fp)
    mkLabel dt = FileLabel FileAdded (ClientIdent "unknown") dt Nothing


withTimedFSTree   :: (TimedFSTree FileLabel -> TimedFSTree FileLabel)
                  -> FSState -> FSState
withTimedFSTree f = FSState . f . fsStateTree

--------------------------------------------------------------------------------

updateNotification   :: Notification -> FSState -> FSState
updateNotification n = withTimedFSTree (FSTree . f p . unTree)
  where
    f = case kind . event $ n of
         FileAdded        -> flip addFileAt newFile
         FileRemoved      -> (\p -> deleteFileAt p fName (Max dt))
         FileUpdated      -> flip updateFileAt g
         DirectoryAdded   -> flip addDirectoryAt newDir
         DirectoryRemoved -> (\p -> deleteDirectoryAt p fName (Max dt))

    g file = file { fileData = fData }

    fData   = FileData (fromNotification n) dt
    dt      = N.timestamp n
    newFile = File fName fData (measure fData)
    newDir  = emptyDirectory fName fData


    p'          = affectedPath . event $ n
    (p'',fName) = andLast $ subPath p'
    p           = (unUI $ owner p') : p''
