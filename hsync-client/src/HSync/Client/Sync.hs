{-# Language  OverloadedStrings
            , TypeFamilies
  #-}
module HSync.Client.Sync where

import Data.Default


import HSync.Client.Import

import Network.HTTP.Conduit(Manager)


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
               , localBaseDir    = "/home/frank/tmp"
               , serverAddress   = "http://localhost:3000"
               , user            = "nobody"
               , hashedPassword  = "hashed-secret"
               , remoteBaseDir   = ""
               , presistentState = ()
               , clientIdent     = "client-ident"
               -- Database
               }
