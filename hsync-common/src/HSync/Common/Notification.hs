{-# Language TemplateHaskell    #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language DeriveDataTypeable #-}
module HSync.Common.Notification(-- Events
                                  EventKind(..)
                                , involvesFile, involvesDirectory
                                , Event(..)
                                , fileAdded , fileRemoved, fileUpdated
                                , directoryAdded, directoryRemoved

                                -- Notifications
                                , Notification(..)
                                , toLog
                                , matchesNotification
                                ) where

import Data.ByteString(ByteString)

import Data.Aeson.TH
import Data.Data(Data, Typeable)
import Data.List(intercalate)
import Data.SafeCopy(base, deriveSafeCopy)

import HSync.Common.Types
import HSync.Common.DateTime(DateTime)
import HSync.Common.FileIdent

import qualified Data.ByteString.Char8 as B

--------------------------------------------------------------------------------

data EventKind = FileAdded
               | FileRemoved
               | FileUpdated
               | DirectoryAdded
               | DirectoryRemoved
               deriving (Show,Read,Eq,Data,Typeable)

$(deriveJSON defaultOptions ''EventKind)
$(deriveSafeCopy 0 'base ''EventKind)


involvesFile   :: EventKind -> Bool
involvesFile k = k `elem` [FileAdded, FileRemoved, FileUpdated]

involvesDirectory :: EventKind -> Bool
involvesDirectory = not . involvesFile






data Event = Event { kind              :: EventKind
                   , affectedPath      :: Path
                   , affectedFileIdent :: Maybe FileIdent
                   }
             deriving (Show,Read,Eq,Data,Typeable)

$(deriveJSON defaultOptions ''Event)
$(deriveSafeCopy 0 'base ''Event)


fileAdded        :: Path -> Event
fileRemoved      :: Path -> FileIdent -> Event
fileUpdated      :: Path -> FileIdent -> Event

fileAdded   p    = Event FileAdded p Nothing
fileRemoved p fi = Event FileRemoved p (Just fi)
fileUpdated p fi = Event FileUpdated p (Just fi)

directoryAdded        :: Path -> Event
directoryRemoved      :: Path -> FileIdent -> Event


directoryAdded   p    = Event DirectoryAdded p Nothing
directoryRemoved p fi = Event DirectoryRemoved p (Just fi)


--------------------------------------------------------------------------------

data Notification = Notification { event     :: Event
                                 , changee   :: ClientIdent
                                 , timestamp :: DateTime
                                 }
                  deriving (Read,Eq,Show,Data,Typeable)

$(deriveJSON defaultOptions ''Notification)
$(deriveSafeCopy 0 'base ''Notification)

toLog                          :: Notification -> String
toLog (Notification evt ci ti) = intercalate ":" $ [show ti, show ci, show evt]

fromLog :: ByteString -> Maybe Notification
fromLog = const Nothing --TODO: Implement this

matchesNotification       :: Path -> Notification -> Bool
p `matchesNotification` n = (affectedPath . event $ n) `isSubPathOf` p


-- evtS = "FileAdded (Path \"\" [])"

-- testS = B.pack "2013-08-23-18:40:59.975000000000-UTC:\"\":FileAdded (Path \"\" [])"
