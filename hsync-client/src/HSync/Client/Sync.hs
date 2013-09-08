{-# Language  OverloadedStrings
            , TypeFamilies
  #-}
module HSync.Client.Sync where

import Data.Default
import Data.List(intercalate)

import HSync.Client.Import

import Network.HTTP.Conduit(Manager)

import Yesod.Client

import qualified Data.Text as T
import qualified Data.List


--------------------------------------------------------------------------------

type ServerAddress = Text
type PersistentState = ()
type PartialPath = Text


-- | Each Synchronized Tree has its own manager/settings etc
data Sync = Sync { httpManager     :: Manager
                 , localBaseDir    :: FilePath -- without trailing /
                 , serverAddress   :: ServerAddress
                 , user            :: UserIdent
                 , hashedPassword  :: HashedPassword
                 , remoteBaseDir   :: PartialPath
                 , presistentState :: PersistentState
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
               , presistentState = ()
               , clientIdent     = "client-ident"
               -- Database
               }


toLocalPath               :: Sync -> Path -> FilePath
toLocalPath s (Path _ ps) = intercalate "/" . (localBaseDir s :) . map T.unpack $ ps



-- | given a local file path, create a (remote) Path corresponding to it
--
-- Precondition: localBaseDir cli is a basedir of the given file path.
-- this is not checked.
toRemotePath        :: Sync -> FilePath -> Path
toRemotePath cli fp = let n   = length . localBaseDir $ cli
                          fp' = Data.List.drop (n+1) fp
                          p   = T.split (== '/') . T.pack $ fp'
                      in Path (user cli) p



--------------------------------------------------------------------------------
-- | A sync is YesodClient

type ActionT = YesodClientMonadT Sync


getSync :: Monad m => ActionT m Sync
getSync = clientInstance



runActionT          :: Functor m => ActionT m a -> Sync -> m a
runActionT act sync = evalYesodClientT act sync def


instance IsYesodClient Sync where
    type YesodServer Sync = HSyncServer
    serverAppRoot = serverAddress
    server   _    = def
    manager       = httpManager
