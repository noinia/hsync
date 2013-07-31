{-# Language  OverloadedStrings
  #-}
module HSync.Common.Types( UserIdent
                         , Password
                         , HashedPassword
                         , ClientIdent
                         -- DateTime
                         , DateTime(..)
                         , currentTime
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

import Control.Applicative((<$>))
import Data.Text(Text)
import Yesod.Core

import Data.List(intercalate, isPrefixOf)

import HSync.Common.Import(showT)


import qualified Data.Text as T

-- import System.Locale
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format
-- import qualified Data.Time.Format as D

--readT = read . T.unpack

--------------------------------------------------------------------------------

type UserIdent = Text
type Password = Text
type HashedPassword = Text

type ClientIdent = Text


--------------------------------------------------------------------------------

newtype DateTime = DateTime UTCTime
    deriving (Show,Read,Eq,Ord)

instance ParseTime DateTime where
    buildTime tl = DateTime . buildTime tl


currentTime :: IO DateTime
currentTime = DateTime <$> getCurrentTime


-- dateTimeFormat :: String
-- dateTimeFormat = "%0C%F-%T"

instance PathPiece DateTime where
    toPathPiece (DateTime t) = showT t
    fromPathPiece s          = case reads . T.unpack $ s of
                                 ((t,""):_) -> Just $ DateTime t
                                 _          -> Nothing
-- type DateTime = Text

--------------------------------------------------------------------------------



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
