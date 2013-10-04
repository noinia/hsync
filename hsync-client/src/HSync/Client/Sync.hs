{-# Language  OverloadedStrings
            , TypeFamilies
  #-}
module HSync.Client.Sync where

import Data.Default
import Data.List(intercalate)

import HSync.Client.Import

import HSync.Common.DateTime(DateTime)
import HSync.Common.FSTree

import Network.HTTP.Conduit(Manager)

import qualified Data.Text as T
import qualified Data.List


--------------------------------------------------------------------------------

type ServerAddress = Text
type PersistentState = FSTree DateTime
type PartialPath = Text


-- | Each Synchronized Tree has its own manager/settings etc
data Sync = Sync { httpManager     :: Manager
                 , localBaseDir    :: FilePath -- without trailing /
                 , serverAddress   :: ServerAddress
                 , user            :: UserIdent
                 , hashedPassword  :: HashedPassword
                 , remoteBaseDir   :: PartialPath
                 , persistentState :: PersistentState
                 , clientIdent     :: ClientIdent
                 -- Database
                 }


instance Default Sync where
    def = Sync { httpManager     = undefined
               , localBaseDir    = "/Users/frank/tmp/synced"
               , serverAddress   = "http://localhost:3000"
               , user            = "nobody"
               , hashedPassword  = "hashed-secret"
               , remoteBaseDir   = ""
               , persistentState = NoFiles
               , clientIdent     = "client-ident"
               -- Database
               }


remoteTree :: Sync -> FSTree DateTime
remoteTree = persistentState

toLocalPath               :: Sync -> Path -> FilePath
toLocalPath s (Path _ ps) = toLocalPath' s ps

toLocalPath'   :: Sync -> SubPath -> FilePath
toLocalPath' s = intercalate "/" . (localBaseDir s :) . map T.unpack


-- | given a local file path, create a (remote) Path corresponding to it
--
-- Precondition: localBaseDir cli is a basedir of the given file path.
-- this is not checked.
toRemotePath        :: Sync -> FilePath -> Path
toRemotePath cli fp = let n   = length . localBaseDir $ cli
                          fp' = Data.List.drop (n+1) fp
                          p   = T.split (== '/') . T.pack $ fp'
                      in toRemotePath' cli p


toRemotePath'   :: Sync -> SubPath -> Path
toRemotePath' s = Path (user s)
