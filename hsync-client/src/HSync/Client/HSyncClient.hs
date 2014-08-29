{-# Language GeneralizedNewtypeDeriving #-}
{-# Language DeriveDataTypeable #-}
module HSync.Client.HSyncClient where

import Prelude hiding (FilePath)

import Control.Applicative

import Control.Monad(mzero)


import Data.Data(Data, Typeable)
import Data.Text(Text, unpack)
import Data.Yaml

import System.Log.Logger(Priority(..))

import Filesystem.Path.CurrentOS(FilePath)

import HSync.Client.Import
import HSync.Client.SyncActions(SyncMode(..))
import HSync.Client.Sync(readYamlConfig)

import qualified Filesystem.Path.CurrentOS as FP

--------------------------------------------------------------------------------

data SyncInfo = SyncInfo { name     :: Text
                         , syncMode :: SyncMode
                         }
                deriving (Show,Read,Eq,Data,Typeable)


instance FromJSON SyncInfo where
  parseJSON (Object v) = SyncInfo <$> v .: "name"
                                  <*> v .: "mode"
  parseJSON _          = mzero


data HSyncSettings = HSyncSettings { syncConfigDir :: FilePath
                                   , syncs         :: [SyncInfo]
                                   , logLevel      :: Priority
                                   , logDirectory  :: FilePath
                                   }
                     deriving (Show,Eq)

instance FromJSON Priority where
  parseJSON (String s) = case reads . unpack $ s of
                           (p,"") : _ -> pure p
                           _          -> mzero
  parseJSON _          = mzero


instance FromJSON HSyncSettings where
  parseJSON (Object v) = HSyncSettings <$> (FP.decode <$> v .: "syncConfigDir")
                                       <*>                v .: "syncs"
                                       <*>                v .: "logLevel"
                                       <*> (FP.decode <$> v .: "logDirectory")
  parseJSON _          = mzero

readConfig :: FilePath -> IO (Either ErrorMessage HSyncSettings)
readConfig = readYamlConfig


-- | The global application
data HSyncClient = HSyncClient { globalSettings :: HSyncSettings
                               }

readHSync fp = (fmap HSyncClient) <$> readConfig fp


-- runSyncs hsc = do


-- main = do
--          hs <- readHSync $ FP.decode "config/hsync.yaml"
--          print "woei"
