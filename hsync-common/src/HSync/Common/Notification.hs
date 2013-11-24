{-# LANGUAGE TemplateHaskell    #-}
{-# Language  OverloadedStrings #-}
module HSync.Common.Notification(-- Events
                                  EventKind(..)
                                , affectedPaths
                                -- Notifications
                                , Notification(..)
                                , toLog
                                , matchesNotification
                                ) where

import Data.ByteString(ByteString)

import Data.Aeson.TH

import Data.List(intercalate)

import HSync.Common.Types
import HSync.Common.DateTime(DateTime)
import HSync.Common.FileIdent

import qualified Data.ByteString.Char8 as B

--------------------------------------------------------------------------------

data EventKind = FileAdded        Path
               | FileRemoved      Path FileIdent
               | FileUpdated      Path FileIdent
               | FileMoved        Path FileIdent Path
               | DirectoryAdded   Path
               | DirectoryRemoved Path FileIdent
               | DirectoryUpdated Path
               | DirectoryMoved   Path Path
               deriving (Show,Read,Eq)

$(deriveJSON id ''EventKind)

affectedPaths                        :: EventKind -> [Path]
affectedPaths (FileAdded p)          = [p]
affectedPaths (FileRemoved p _)      = [p]
affectedPaths (FileUpdated p _)      = [p]
affectedPaths (FileMoved f _ t)      = [f,t]
affectedPaths (DirectoryAdded p)     = [p]
affectedPaths (DirectoryRemoved p _) = [p]
affectedPaths (DirectoryUpdated p)   = [p]
affectedPaths (DirectoryMoved f t)   = [f,t]


affectedFileIdent (FileRemoved _ fi)      = Just fi
affectedFileIdent (FileUpdated _ fi)      = Just fi
affectedFileIdent (FileMoved _ fi _)      = Just fi
affectedFileIdent (DirectoryRemoved _ fi) = Just fi
affectedFileIdent _                       = Nothing


data Notification = Notification { event     :: EventKind
                                 , changee   :: ClientIdent
                                 , timestamp :: DateTime
                                 }
                  deriving (Read,Eq,Show)

$(deriveJSON id ''Notification)

toLog                          :: Notification -> String
toLog (Notification evt ci ti) = intercalate ":" $ [show ti, show ci, show evt]

fromLog :: ByteString -> Maybe Notification
fromLog = const Nothing --TODO: Implement this

matchesNotification       :: Path -> Notification -> Bool
p `matchesNotification` n = let ps = affectedPaths . event $ n in
                            any (`isSubPathOf` p) ps





evtS = "FileAdded (Path \"\" [])"

testS = B.pack "2013-08-23-18:40:59.975000000000-UTC:\"\":FileAdded (Path \"\" [])"
