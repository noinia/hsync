{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Yesod.Client where

import Control.Applicative((<$>),(<*>),Applicative(..))

import Blaze.ByteString.Builder( -- Builder
                               -- , fromByteString
                               -- , flush
                                toLazyByteString
                               )


import Control.Monad.Reader(MonadReader(..),ReaderT,runReaderT)
import Control.Monad.State( MonadState(..)
                          , StateT(..)
                          , runStateT
                          , modify
                          )

import Control.Monad.Catch(MonadThrow(..))
import Control.Monad.Trans.Resource(ResourceT)

import Data.ByteString(ByteString, empty)

import Data.Conduit(Source, ResumableSource, mapOutput, transPipe)
import Data.Default
import Data.Monoid((<>))
import Data.Text(Text)




import Network.HTTP.Conduit( Request
                           , Response
                           , Manager
                           , CookieJar
                           , RequestBody(..)
                           -- , HttpException(..)
                           , http
                           , parseUrl
                           , createCookieJar
                           -- , withManager
                           , method
                           , requestBody
                           , requestBodySourceChunked
                           , responseCookieJar
                           )

import Network.HTTP.Types

import Yesod.Core

import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Network.HTTP.Conduit       as HC

import qualified Network.HTTP.Types.Header  as H

import qualified Yesod.JoinPath             as Y

--------------------------------------------------------------------------------

-- | Specifies what type of operations a yesodClient should support
class IsYesodClient client where
    type YesodServer client

    serverAppRoot  :: client -> Text
    manager        :: client -> Manager
    server         :: client -> YesodServer client

    defaultRequestModifier   :: client -> Request -> Request
    defaultRequestModifier _ = id

--------------------------------------------------------------------------------
-- | Basic actions that we can run on something that is a MonadYesodClient

class ( MonadResource m
      , MonadBaseControl IO m
      , IsYesodClient client
      , RenderRoute (YesodServer client)
      ) =>
      MonadYesodClient client yt m | yt -> client where

  runGetRoute :: Route (YesodServer client) ->
                 yt m (Response (ResumableSource m ByteString))

  runPostRoute :: Route (YesodServer client) ->
                   Source (ResourceT IO) ByteString ->
                   yt m (Response (ResumableSource m ByteString))


  runDeleteRoute :: Route (YesodServer client) ->
                     yt m (Response (ResumableSource m ByteString))




--------------------------------------------------------------------------------

-- | A monad transformer that implements the MonadYesodClient actions
newtype YesodClientT cli m a =
    YesodClientT { unYCT :: StateT YesodClientState (ReaderT cli m) a }
    deriving (Monad, Functor,Applicative, MonadIO, MonadThrow)


-- -- | A yesodClientMonadT is a monad.
-- instance Monad m => Monad (YesodClientT cli m) where
--     return                      = YesodClientT . return
--     (YesodClientT m) >>= f = let f' = unYCT . f in
--                                   YesodClientT $ m >>= f'

-- -- | ... a functor,
-- instance Functor m => Functor (YesodClientT cli m) where
--     fmap f = YesodClientT . fmap f . unYCT

-- -- | ... an applicative,
-- instance (Functor m, Monad m) => Applicative (YesodClientT cli m) where
--     pure = YesodClientT . pure
--     (YesodClientT f) <*> (YesodClientT x) = YesodClientT $ f <*> x

-- | .. a monad transformer,
instance MonadTrans (YesodClientT cli) where
    lift = YesodClientT . lift . lift

-- | a reader monad.
instance Monad m => MonadReader cli (YesodClientT cli m) where
    ask     = YesodClientT . lift $ ask
    local f = YesodClientT . StateT . fmap (local f) . runStateT . unYCT


-- --- and we can do IO if required.
-- instance MonadIO m => MonadIO (YesodClientT cli m) where
--     liftIO = YesodClientT . liftIO




--------------------------------------------------------------------------------

-- | The state maintained by a YesodClient
data YesodClientState = YesodClientState { cookieJar'      :: Maybe CookieJar
                                         }
                         deriving (Show,Eq)

instance Default YesodClientState where
    def = YesodClientState . Just . createCookieJar $ []

-- | YesodClientT maintains YesodClientState as state
instance Monad m => MonadState YesodClientState (YesodClientT cli m) where
    state = YesodClientT . state

--------------------------------------------------------------------------------

-- | Run a yesodClientT monad transformer
runYesodClientT                                :: YesodClientT cli m a ->
                                                  cli ->
                                                  YesodClientState ->
                                                      m (a,YesodClientState)
runYesodClientT (YesodClientT comp) cli s = runReaderT (runStateT comp s) cli


-- | run an action, then drop the client state
evalYesodClientT            :: Functor m => YesodClientT cli m a ->
                                 cli -> YesodClientState -> m a
evalYesodClientT comp cli s = fst <$> runYesodClientT comp cli s


-- | Get the client instance
clientInstance :: Monad m => YesodClientT cli m cli
clientInstance = ask

-- | Get the cookiejar
cookieJar :: (Monad m, Functor m) => YesodClientT cli m (Maybe CookieJar)
cookieJar = cookieJar' <$> get


-- | Update the cookiejar
updateCookieJar   :: Monad m => Response body => YesodClientT cli m ()
updateCookieJar r = modify $ \st -> st {cookieJar' = Just . responseCookieJar $ r}

--------------------------------------------------------------------------------

-- | The MonadYesodClient instance for the YesodClientT
instance ( MonadResource m
         , MonadBaseControl IO m
           -- client `IsYesodClientFor` (YesodServer client):
         , IsYesodClient client
         , RenderRoute (YesodServer client)
         , Y.HasJoinPath (YesodServer client)
         ) =>
         MonadYesodClient client (YesodClientT client) m where

  runGetRoute = flip runRouteWith id

  runPostRoute r s = runRouteWith r $ \req ->
                       req { method      = methodPost
                           , requestBody = requestBodySourceChunked s
                           }

  runDeleteRoute r = runRouteWith r $ \req ->
                       req { method      = methodDelete
                           , requestBody = RequestBodyBS empty
                           }

--------------------------------------------------------------------------------
-- | Relating the Client to the server

type IsYesodClientFor client server = ( IsYesodClient client
                                      , RenderRoute server
                                      , Y.HasJoinPath server
                                      , YesodServer client ~ server
                                      )


toUrl     :: (client `IsYesodClientFor` server) =>
             client -> Route server -> String
toUrl cli r = let root     = serverAppRoot cli
                  (pcs,qs) = renderRoute r
                  urlBldr  = Y.joinPath (server cli) root pcs qs in
              LC.unpack . toLazyByteString $ urlBldr



toReq   :: (client `IsYesodClientFor` server,
            Functor m,
            MonadThrow m ) =>
           Route server -> YesodClientT client m Request
toReq r = do
  cli <- clientInstance
  mcj <- cookieJar
  req <- parseUrl . toUrl cli $ r
  return . defaultRequestModifier cli $ req { HC.cookieJar       = mcj
                                            , HC.responseTimeout = Nothing
                                            }


-- | Given a route and a request modification function f. Create a request for
-- this route, modify it with f, and then run the request.
runRouteWith   :: ( client `IsYesodClientFor` server
               , MonadResource m, MonadBaseControl IO m
               ) =>
               Route server ->
               (Request -> Request) ->
                   YesodClientT client m (Response (ResumableSource m ByteString))
runRouteWith r f = do
  mgr <- manager <$> clientInstance
  req <- f <$> toReq r
  lift $ http req mgr


addRequestHeader         :: H.Header -> Request -> Request
addRequestHeader hdr req = req { HC.requestHeaders = hdr : HC.requestHeaders req }
