{-# Language  OverloadedStrings
            , TypeFamilies
            , TypeSynonymInstances
            , FlexibleContexts
  #-}
module Main where





import Creds




import HSync.Client.Import
import HSync.Client.Sync

--import HSync.Server.Foundation(HSyncServer)

import HSync.Server.Handler.Auth
import HSync.Server.Application


import Control.Concurrent(forkIO)
import Control.Failure
import Control.Monad(when)
-- import Control.Monad.State.Class
-- import Control.Monad.Reader.Class

import Control.Monad.IO.Class (liftIO)

import Data.ByteString(ByteString)
import Data.Conduit(Source)
import Data.Conduit.Binary
import Data.Default

import Network.HTTP.Conduit( Request
                           , Response
                           , Manager
                           , CookieJar
                           , RequestBody(..)
                           , HttpException(..)
--                           , createCookieJar
                           , withManager
                           , requestBody
                           , responseBody
                           , responseStatus
                           , responseCookieJar
                           )
import Network.HTTP.Types

import Network

import System.Environment (getArgs)

import Yesod.Client -- ( IsYesodClient(..)
                   -- , YesodClientMonadT(..)
                   -- , runYesodClientT
                   -- , runRouteWith, runGetRoute, runPostRoute
                   -- )

-- import Yesod



--import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Conduit               as C
import qualified Network.HTTP.Conduit       as HC
import qualified Data.Text                  as T
import qualified Data.List

--------------------------------------------------------------------------------

type GlobalSettings = Text
type InstanceSettings = Text

--------------------------------------------------------------------------------

-- | The global application
data HSyncClient = HSyncClient { globalSettings :: GlobalSettings
                               }


-- | given a local file path, create a (remote) Path corresponding to it
--
-- Precondition: localBaseDir cli is a basedir of the given file path.
-- this is not checked.
toRemotePath        :: Sync -> FilePath -> Path
toRemotePath cli fp = let n   = length . localBaseDir $ cli
                          fp' = Data.List.drop (n+1) fp
                          p   = T.split (== '/') . T.pack $ fp'
                      in Path (user cli) p


remoteFileInfo    :: MonadIO m => FilePath -> ActionT m (FileIdent,Path)
remoteFileInfo fp = do
  sync <- getSync
  fi   <- liftIO $ fileIdent fp
  return (fi,toRemotePath sync fp)


-- instanceState      :: Sync -> InstanceState Sync
-- instanceState inst = InstanceState { clientInstance = inst
--                                    , cookieJar      = createCookieJar []
--                                    }



-- | maintain mutable state and imutable state


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

setSessionCreds :: Monad m => Response body -> ActionT m ()
setSessionCreds = updateCookieJar


login :: ( MonadResource m, Failure HttpException m
         , MonadBaseControl IO m) => ActionT m Bool
login = do
  sync <- getSync
  resp <- runGetRoute $ MyLoginR (user sync) (hashedPassword sync)
  body <- lift $ responseBody resp C.$$+- sinkLbs
  case LC.unpack body of
    "VALID"   -> setSessionCreds resp >> return True
    "INVALID" -> return False


putFile    :: ( MonadResource m, Failure HttpException m
              , MonadIO m, MonadBaseControl IO m) => FilePath -> ActionT m ()
putFile fp = do
  (fi,p) <- remoteFileInfo fp
  let h = PutFileR fi p
      s = sourceFile fp
  sync <- getSync
  liftIO $ print $ toUrl sync h
  resp <- runPostRoute h s
  liftIO $ print "woei"


getFile   :: ( MonadResource m, Failure HttpException m
         , MonadBaseControl IO m) => Path -> ActionT m ()
getFile p = do
  sync <- getSync
  resp <- runGetRoute $ FileR p
  let fp     = toLocalPath sync p
      status = responseStatus resp
  when (status == ok200) $ lift (responseBody resp C.$$+- sinkFile fp)


main :: IO ()
main = withSocketsDo $ withManager $ \mgr -> do
         let sync = def { httpManager    = mgr
                        , user           = myUser
                        , hashedPassword = myHashedPass
                        }
         x <- flip runActionT sync $ do
                loggedIn <- login
                when loggedIn $ putFile "/Users/frank/tmp/synced/test_put.jpg"
                -- when loggedIn $ getFile $ Path (user sync) ["test.jpg"]
                return loggedIn
         liftIO $ print x

--          case parseUrl urlString of
--            Nothing  -> liftIO $ putStrLn "Invalid URL"
--            Just req -> do
--                          let reqHead = req { method = "GET" }
--                          resp <- http reqHead manager
--                          liftIO $ print $ responseStatus resp
--                          liftIO $ mapM_ print $ responseHeaders resp
--                          responseBody resp C.$$+- sinkFile "google.html"
