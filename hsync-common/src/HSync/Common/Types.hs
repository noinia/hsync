module HSync.Common.Types where

import Prelude

import Data.Text(Text)
import Yesod.Core

import Data.List(intercalate, isPrefixOf)

import qualified Data.Text as T

-- import System.Locale
-- import Data.Time (UTCTime)
-- import qualified Data.Time.Format as D

--------------------------------------------------------------------------------

type UserIdent = Text
type Password = Text
type HashedPassword = Text

type ClientIdent = Text


--------------------------------------------------------------------------------

-- newtype DateTime = DateTime UTCTime
--     deriving (Show,Eq,Ord)

-- instance ParseTime DateTime where
--     buildTime tl = DateTime . buildTime tl


-- dateTimeFormat :: String
-- dateTimeFormat = "%0C%F-%T"

-- instance PathPiece DateTime where
--     toPathPiece (DateTime t) = T.pack . formatTime defaultTimeLocale dateTimeFormat $ t
--     fromPathPiece = D.parseTime defaultTimeLocale dateTimeFormat . T.unpack




type DateTime = Text

--------------------------------------------------------------------------------

type FileIdent = Text

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
                  deriving (Show,Read,Eq)


toLog                          :: Notification -> String
toLog (Notification evt ci ti) = intercalate ":" $ [show ti, show ci, show evt]

matchesNotification       :: Path -> Notification -> Bool
p `matchesNotification` n = let ps = affectedPaths . event $ n in
                            any (`isSubPathOf` p) ps
