{-# Language OverloadedStrings #-}
module Main where


import Network

import Control.Concurrent(forkIO)

import Network.HTTP.Conduit

import Control.Monad.State

import Data.Conduit.Binary

import System.Environment (getArgs)

import Control.Monad.IO.Class (liftIO)

import Creds

import Data.Text

import qualified Data.ByteString.Lazy as L
import qualified Data.Conduit as C

--------------------------------------------------------------------------------

type GlobalSettings = Text
type InstanceSettings = Text

protocol = "http://"
urlString = protocol ++ "localhost:3000/login/" ++ user ++ "/" ++ hashedPass


--------------------------------------------------------------------------------

-- | The global application
data HSyncClient = HSyncClient { globalSettings :: GlobalSettings
                               }

type PersistentState = ()

-- | Each Synchronized Tree has its own manager/settings etc
data ClientInstance = ClientInstance { httpManager     :: Manager
                                     , localBaseDir    :: FilePath
                                     , remoteBase      :: String
                                     , presistentState :: PersistentState
                                     -- Database
                                     }


type InstanceState = ()

newtype GHandler m a = Handler (StateT InstanceState m a)

type Handler = GHandler IO


runInstance ci = do
                   loadPersistent
                   login
                   from <- return "now" -- TODO get the data from loadPersistent
                   forkIO $ listenRemote ci from
                   forkIO $ listenLocal ci


loadPersistent = return ()
login = undefined

listenRemote = undefined
listenLocal = undefined





--------------------------------------------------------------------------------

main :: IO ()
main = withSocketsDo $ withManager $ \manager -> do
         case parseUrl urlString of
           Nothing  -> liftIO $ putStrLn "Invalid URL"
           Just req -> do
                         let reqHead = req { method = "GET" }
                         resp <- http reqHead manager
                         liftIO $ print $ responseStatus resp
                         liftIO $ mapM_ print $ responseHeaders resp
                         responseBody resp C.$$+- sinkFile "google.html"
