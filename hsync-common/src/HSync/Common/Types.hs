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

import Control.Applicative((<$>),(<*>))
import Data.Text(Text)
import Yesod.Core

import Data.ByteString(ByteString)
import Data.Function(on)
import Data.List(intercalate, isPrefixOf)

import HSync.Common.Import(showT)



-- import System.Locale
import Data.Time (Day, UTCTime, getCurrentTime , utctDay)
import Data.Time.Format

--import System.Locale(default)

import Text.Read(readMaybe)



import qualified Data.ByteString.Char8 as B
import qualified Data.Time.Format as D
import qualified Data.Text as T

--readT = read . T.unpack

--------------------------------------------------------------------------------

type UserIdent = Text
type Password = Text
type HashedPassword = Text

type ClientIdent = Text


--------------------------------------------------------------------------------

newtype DateTime = DateTime { unDT :: UTCTime }
    deriving (Eq,Ord)


dtPrefix = "DateTime "

instance Show DateTime where
    show (DateTime t) = dtPrefix ++ showDateTime t

instance Read DateTime where
    readsPrec _ = readDateTime . drop (length dtPrefix)

instance ParseTime DateTime where
    buildTime tl = DateTime . buildTime tl

showDateTime :: UTCTime -> String
showDateTime = formatTime undefined dateTimeFormat

readDateTime :: ReadS DateTime
readDateTime = readsTime undefined dateTimeFormat

dateTimeFormat :: String
dateTimeFormat = "%F-%T.%q-%Z"

instance PathPiece DateTime where
    toPathPiece = T.pack . showDateTime . unDT
    fromPathPiece = D.parseTime undefined dateTimeFormat . T.unpack

currentTime :: IO DateTime
currentTime = DateTime <$> getCurrentTime

day :: DateTime -> Day
day = utctDay . unDT

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
