{-# Language  OverloadedStrings
            , TypeFamilies
  #-}
module HSync.Client.Sync where

import Data.Default
import Data.List(intercalate)

import HSync.Client.Import

import Network.HTTP.Conduit(Manager)

import qualified Data.Text as T


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
