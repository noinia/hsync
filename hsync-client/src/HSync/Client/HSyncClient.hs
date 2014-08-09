{-# Language GeneralizedNewtypeDeriving #-}
module HSync.Client.HSyncClient where

import Prelude hiding (FilePath)

import Control.Applicative

import Control.Monad(mzero)


import Data.Data(Data, Typeable)
import Data.Text(Text)
import Data.Yaml

import System.Log.Logger(Priority)

import Filesystem.Path.CurrentOS(FilePath)

import HSync.Client.SyncActions(SyncMode(..))

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
                     deriving (Show,Read,Eq,Data,Typeable)



-- | The global application
data HSyncClient = HSyncClient { globalSettings :: HSyncSettings
                               }


-- clientMain fp = return ()
