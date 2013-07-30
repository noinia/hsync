{-# Language  OverloadedStrings
            , TypeFamilies
  #-}
module Main where


import Blaze.ByteString.Builder( Builder
                               , fromByteString
                               , flush
                               , toLazyByteString
                               )


import Creds

import Network.HTTP.Types

import HSync.Client.Import
--import HSync.Server.Foundation(HSyncServer)

import HSync.Server.Handler.Auth
import HSync.Server.Application


import Control.Concurrent(forkIO)
import Control.Monad.State
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
                           , http
                           , parseUrl
                           , createCookieJar
                           , withManager
                           , method
                           , requestBody
                           , responseBody
                           , responseCookieJar
                           )
import Network

import System.Environment (getArgs)




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

type ServerAddress = Text
type PersistentState = ()
type PartialPath = Text


-- | Each Synchronized Tree has its own manager/settings etc
data ClientInstance = ClientInstance { httpManager     :: Manager
                                     , localBaseDir    :: FilePath -- without trailing /
                                     , serverAddress   :: ServerAddress
                                     , user            :: UserIdent
                                     , hashedPassword  :: HashedPassword
                                     , remoteBaseDir   :: PartialPath
                                     , presistentState :: PersistentState
                                     , clientIdent     :: ClientIdent
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
                         , clientIdent     = "client-ident"
                         -- Database
                         }


-- | given a local file path, create a (remote) Path corresponding to it
--
-- Precondition: localBaseDir cli is a basedir of the given file path.
-- this is not checked.
toRemotePath        :: ClientInstance -> FilePath -> Path
toRemotePath cli fp = let n   = length . localBaseDir $ cli
                          fp' = Data.List.drop (n+1) fp
                          p   = T.split (== '/') . T.pack $ fp
                      in Path (user cli) p

data InstanceState inst = InstanceState { clientInstance :: inst
                                        , cookieJar      :: CookieJar
                                        }
                        deriving (Show,Eq)



instanceState      :: ClientInstance -> InstanceState ClientInstance
instanceState inst = InstanceState { clientInstance = inst
                                   , cookieJar      = createCookieJar []
                                   }




type ActionT inst m a = StateT (InstanceState inst) m a


type Action m a = ActionT ClientInstance m a


getClientInstance = clientInstance <$> get



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


-- class ClientActionMonad m where
--     type ClientInstance m






toReq r = do
  cli <- getClientInstance
  cj  <- cookieJar <$> get
  req <- lift . parseUrl . toUrl cli $ r
  return $ req { HC.cookieJar = Just cj }


runGetReq r = do
  mgr <- httpManager <$> getClientInstance
  req <- toReq r
  http req mgr


-- runPostReq :: Handle HSyncServer ->

runPostReq r s = do
  mgr <- httpManager <$> getClientInstance
  req' <- toReq r
  let req = req' { method      = methodPost
                 , requestBody = RequestBodySourceChunked . toBuilder $ s}
  http req mgr


toBuilder :: Monad m => Source m ByteString -> Source m Builder
toBuilder = C.mapOutput (\bs -> fromByteString bs <> flush)



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

setSessionCreds      :: Monad m => Response body -> Action m ()
setSessionCreds resp = modify (\s -> s {cookieJar = responseCookieJar resp })


login = do
  cli  <- getClientInstance
  resp <- runGetReq $ MyLoginR (user cli) (hashedPassword cli)
  body <- responseBody resp C.$$+- sinkLbs
  case LC.unpack body of
    "VALID"   -> setSessionCreds resp >> return True
    "INVALID" -> return False

putFile fp = do
  cli  <- getClientInstance
  let h = PutFileR . toRemotePath cli $ fp
      s = sourceFile fp
  resp <- runPostReq h s
  liftIO $ print "woei"






main :: IO ()
main = withSocketsDo $ withManager $ \manager -> do
         let cli = def { httpManager = manager }
         (x,_) <- runAction login $ instanceState cli
         liftIO $ print x

--          case parseUrl urlString of
--            Nothing  -> liftIO $ putStrLn "Invalid URL"
--            Just req -> do
--                          let reqHead = req { method = "GET" }
--                          resp <- http reqHead manager
--                          liftIO $ print $ responseStatus resp
--                          liftIO $ mapM_ print $ responseHeaders resp
--                          responseBody resp C.$$+- sinkFile "google.html"
