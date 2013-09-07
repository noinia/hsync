{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module HSync.Common.DateTime( DateTime(..)
                            , currentTime
                            , Day
                            , day

                            , modificationTime
                            ) where



import Control.Applicative((<$>),(<*>))
import Control.Monad(mzero)
import Control.Monad.IO.Class(liftIO, MonadIO(..))

import Data.Aeson

import Data.Time (Day, UTCTime, getCurrentTime , utctDay)
import Data.Time.Format



import System.Directory(getModificationTime)

import Yesod.Core


import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict   as H
import qualified Data.Time.Format      as D
import qualified Data.Text             as T

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

instance ToJSON DateTime where
    toJSON (DateTime d) = object ["DateTime" .= (showDateTime d)]

instance FromJSON DateTime where
    parseJSON (Object (H.toList -> [(k, String v)]))
      | k == "DateTime" = maybe mzero return
                        . D.parseTime undefined dateTimeFormat
                        . T.unpack $ v
    parseJSON _         = mzero

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


-- | Get the file modification time
modificationTime    :: MonadIO m => FilePath -> m DateTime
modificationTime fp = liftIO $ DateTime <$> getModificationTime fp
