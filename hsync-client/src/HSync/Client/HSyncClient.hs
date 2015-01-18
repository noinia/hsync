{-# Language GeneralizedNewtypeDeriving #-}
{-# Language DeriveDataTypeable #-}
module HSync.Client.HSyncClient where

import Prelude hiding (FilePath)

import Control.Applicative

import Control.Monad(mzero)


import Data.Data(Data, Typeable)
import Data.Text(Text, unpack)
import Data.Yaml

import System.Log.Logger( Priority(..)
                        , rootLoggerName
                        , updateGlobalLogger
                        , addHandler
                        )
-- import qualified System.Log.Handler.Syslog as SysLog
import qualified System.Log.Handler.Simple as SimpleLog
import qualified System.Log.Logger as Log

import Filesystem.Path.CurrentOS(FilePath, (</>), encodeString)

import HSync.Client.Import
import HSync.Client.SyncActions(SyncMode(..), launchSyncFrom)
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

readHSync    :: FilePath -> IO (Either ErrorMessage HSyncClient)
readHSync fp = (fmap HSyncClient) <$> readConfig fp


launchSync                    :: HSyncClient -> SyncInfo -> IO ()
launchSync hs (SyncInfo n m) = let dir = syncConfigDir . globalSettings $ hs
                                   fn  = FP.decode n FP.<.> "yaml"
                               in launchSyncFrom (dir FP.</> fn) m


main    :: HSyncClient -> IO ()
main hs = do
  let sets    = globalSettings hs
      logFile = encodeString $ logDirectory sets </> "hsync_client.log"
  fileLog <- SimpleLog.fileHandler logFile (logLevel sets)
  updateGlobalLogger rootLoggerName (addHandler fileLog)
  Log.infoM "HSync.Client.Main" "Starting HSync"
  mapM_ (launchSync hs) $ syncs sets
