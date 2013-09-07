{-# Language  OverloadedStrings
  #-}
module HSync.Common.Types( UserIdent
                         , Password
                         , HashedPassword
                         , ClientIdent
                         -- Paths
                         , Path(..)
                         , toFilePath
                         , isSubPathOf
                         -- Events
                         , EventKind(..)
                         , affectedPaths
                         -- Notifications
                         , Notification(..)
                         , toLog
                         , matchesNotification
                         ) where



import Prelude

import Control.Applicative((<$>),(<*>))
import Data.Text(Text)
import Yesod.Core

import Data.ByteString(ByteString)
import Data.Function(on)
import Data.List(intercalate, isPrefixOf)

import HSync.Common.Import(showT)
import HSync.Common.DateTime(DateTime)

import Text.Read(readMaybe)



import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T

--readT = read . T.unpack

--------------------------------------------------------------------------------

type UserIdent = Text
type Password = Text
type HashedPassword = Text

type ClientIdent = Text

--------------------------------------------------------------------------------

data Path = Path UserIdent [Text]
    deriving (Show,Read,Eq,Ord)

instance PathMultiPiece Path where
    toPathMultiPiece (Path u ps) = u : ps -- map T.pack ps
    fromPathMultiPiece (u:ps) = Just $ Path u ps -- . map T.unpack
    fromPathMultiPiece _      = Nothing

toFilePath                     :: Text -> Path -> FilePath
toFilePath baseDir (Path u ps) = intercalate "/" . map T.unpack $ baseDir:u:ps

isSubPathOf       :: Path -> Path -> Bool
p `isSubPathOf` q = let fp = toFilePath "" p
                        fq = toFilePath "" q in
                    fp `isPrefixOf` fq

--------------------------------------------------------------------------------

data EventKind = FileAdded        Path
               | FileRemoved      Path
               | FileUpdated      Path
               | FileMoved        Path Path
               | DirectoryAdded   Path
               | DirectoryRemoved Path
               | DirectoryUpdated Path
               | DirectoryMoved   Path Path
               deriving (Show,Read,Eq)

affectedPaths                      :: EventKind -> [Path]
affectedPaths (FileAdded p)        = [p]
affectedPaths (FileRemoved p)      = [p]
affectedPaths (FileUpdated p)      = [p]
affectedPaths (FileMoved f t)      = [f,t]
affectedPaths (DirectoryAdded p)   = [p]
affectedPaths (DirectoryRemoved p) = [p]
affectedPaths (DirectoryUpdated p) = [p]
affectedPaths (DirectoryMoved f t) = [f,t]


data Notification = Notification { event     :: EventKind
                                 , changee   :: ClientIdent
                                 , timestamp :: DateTime
                                 }
                  deriving (Read,Eq)


instance Show Notification where
    show (Notification evt ci ti) = intercalate ":" $ [show ti, show ci, show evt]

toLog :: Notification -> String
toLog = show

fromLog :: ByteString -> Maybe Notification
fromLog = const Nothing --TODO: Implement this

matchesNotification       :: Path -> Notification -> Bool
p `matchesNotification` n = let ps = affectedPaths . event $ n in
                            any (`isSubPathOf` p) ps





evtS = "FileAdded (Path \"\" [])"

testS = B.pack "2013-08-23-18:40:59.975000000000-UTC:\"\":FileAdded (Path \"\" [])"
