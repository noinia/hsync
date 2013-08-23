{-# Language  OverloadedStrings
  #-}
module HSync.Common.Types( UserIdent
                         , Password
                         , HashedPassword
                         , ClientIdent
                         -- DateTime
                         , DateTime(..)
                         , currentTime
                         , Day
                         , day
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

import Data.Function(on)
import Data.List(intercalate, isPrefixOf)

import HSync.Common.Import(showT)


import qualified Data.Text as T

-- import System.Locale
import Data.Time (Day, UTCTime, getCurrentTime , utctDay)
import Data.Time.Format
import qualified Data.Time.Format as D

--readT = read . T.unpack

--------------------------------------------------------------------------------

type UserIdent = Text
type Password = Text
type HashedPassword = Text

type ClientIdent = Text


--------------------------------------------------------------------------------

newtype DateTime = DateTime { unDT :: UTCTime }
    deriving (Read,Eq,Ord)

instance Show DateTime where
    show (DateTime t) = formatTime undefined dateTimeFormat $ t


instance ParseTime DateTime where
    buildTime tl = DateTime . buildTime tl

dateTimeFormat :: String
dateTimeFormat = "%F-%T.%q-%Z"


currentTime :: IO DateTime
currentTime = DateTime <$> getCurrentTime

day :: DateTime -> Day
day = utctDay . unDT

instance PathPiece DateTime where
    toPathPiece = T.pack . show
    fromPathPiece = D.parseTime undefined dateTimeFormat . T.unpack


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
