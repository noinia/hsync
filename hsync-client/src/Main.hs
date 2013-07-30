{-# Language  OverloadedStrings
            , TypeFamilies
  #-}
module Main where


import Blaze.ByteString.Builder(toLazyByteString)


import Creds


import HSync.Client.Import
--import HSync.Server.Foundation(HSyncServer)

import HSync.Server.Handler.Auth
import HSync.Server.Application


import Control.Concurrent(forkIO)
import Control.Monad.State
import Control.Monad.IO.Class (liftIO)

import Data.Conduit.Binary
import Data.Default

import Network.HTTP.Conduit
import Network

import System.Environment (getArgs)

-- import Yesod



--import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Conduit               as C

--------------------------------------------------------------------------------

type GlobalSettings = Text
type InstanceSettings = Text

--------------------------------------------------------------------------------

-- | The global application
data HSyncClient = HSyncClient { globalSettings :: GlobalSettings
                               }

type ServerAddress = Text
type PersistentState = ()
type PartialPath = Text


-- | Each Synchronized Tree has its own manager/settings etc
data ClientInstance = ClientInstance { httpManager     :: Manager
                                     , localBaseDir    :: FilePath
                                     , serverAddress   :: ServerAddress
                                     , user            :: UserIdent
                                     , hashedPassword  :: HashedPassword
                                     , remoteBaseDir   :: PartialPath
                                     , presistentState :: PersistentState
                                     -- Database
                                     }


instance Default ClientInstance where
    def = ClientInstance { httpManager     = undefined
                         , localBaseDir    = "/home/frank/tmp"
                         , serverAddress   = "http://localhost:3000"
                         , user            = "nobody"
                         , hashedPassword  = "hashed-secret"
                         , remoteBaseDir   = ""
                         , presistentState = ()
                         -- Database
                         }



type InstanceState inst = inst

type ActionT inst m a = StateT (InstanceState inst) m a


type Action m a = ActionT ClientInstance m a


getClientInstance = get

runAction        :: ActionT inst m a -> InstanceState inst -> m (a,InstanceState inst)
runAction act st = runStateT act st



-- instance Monad (GAction inst m) where
--     return x = Action $ return x
--     -- (Action m) >>= f  = Action $ run


-- login :: Route HSyncServer


class IsYesodClient client where
    type YesodServer client

    serverRoot :: client -> Text
    server     :: client -> YesodServer client


instance IsYesodClient ClientInstance where
    type YesodServer ClientInstance = HSyncServer
    serverRoot = serverAddress
    server   _ = def


toUrl     :: (IsYesodClient client,
             Yesod server,
             YesodServer client ~ server) =>
                client -> Route server -> String
toUrl cli r = let root     = serverRoot cli
                  (pcs,qs) = renderRoute r
                  urlBldr  = joinPath (server cli) root pcs qs in
              LC.unpack . toLazyByteString $ urlBldr


toReq r = getClientInstance >>= \cli -> lift . parseUrl . toUrl cli $ r


runReq r = do
  mgr <- httpManager <$> getClientInstance
  req <- toReq r
  http req mgr







-- runInstance ci = do
--                    loadPersistent
--                    login
--                    from <- return "now" -- TODO get the data from loadPersistent
--                    forkIO $ listenRemote ci from
--                    forkIO $ listenLocal ci


-- loadPersistent = return ()

-- listenRemote = undefined
-- listenLocal = undefined





--------------------------------------------------------------------------------




-- login :: Action m ()
login = do
  cli  <- getClientInstance
  resp <- runReq $ MyLoginR (user cli) (hashedPassword cli)
  responseBody resp C.$$+- sinkFile "/tmp/out"



main :: IO ()
main = withSocketsDo $ withManager $ \manager -> do
         let cli = def { httpManager = manager }
         (x,_) <- runAction login cli
         return x

--          case parseUrl urlString of
--            Nothing  -> liftIO $ putStrLn "Invalid URL"
--            Just req -> do
--                          let reqHead = req { method = "GET" }
--                          resp <- http reqHead manager
--                          liftIO $ print $ responseStatus resp
--                          liftIO $ mapM_ print $ responseHeaders resp
--                          responseBody resp C.$$+- sinkFile "google.html"
