{-# Language   TypeOperators
             , TypeFamilies
             , ConstraintKinds
             , Rank2Types
             , FlexibleContexts
             , FlexibleInstances
             , MultiParamTypeClasses
  #-}
module Yesod.Client where

import Control.Applicative((<$>),(<*>),Applicative(..))

import Blaze.ByteString.Builder( Builder
                               , fromByteString
                               , flush
                               , toLazyByteString
                               )

import Control.Monad.Reader(MonadReader(..),ReaderT,runReaderT)
import Control.Monad.State( MonadState(..)
                          , StateT(..)
                          , runStateT
                          , modify
                          )



import Control.Failure


import Data.ByteString(ByteString, empty)
import Data.Conduit(Source, ResumableSource, mapOutput, ResourceT, transPipe)
import Data.Default
import Data.Monoid((<>))
import Data.Text(Text)




import Network.HTTP.Conduit( Request
                           , Response
                           , Manager
                           , CookieJar
                           , RequestBody(..)
                           , HttpException(..)
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

--------------------------------------------------------------------------------

class IsYesodClient client where
    type YesodServer client

    serverAppRoot :: client -> Text
    manager       :: client -> Manager
    server        :: client -> YesodServer client


-- class YesodClientAction m where
--     type ClientInstance m

--     clientInstance :: m (ClientInstance m)
--     cookieJar      :: m (Maybe CookieJar)

data YesodClientState = YesodClientState { cookieJar'      :: Maybe CookieJar
                                         }
                         deriving (Show,Eq)

instance Default YesodClientState where
    def = YesodClientState . Just . createCookieJar $ []

newtype YesodClientMonadT cli m a =
    YesodClientMonadT { unYCT :: StateT YesodClientState (ReaderT cli m) a }


instance Monad m => Monad (YesodClientMonadT cli m) where
    return                      = YesodClientMonadT . return
    (YesodClientMonadT m) >>= f = let f' = unYCT . f in
                                  YesodClientMonadT $ m >>= f'

instance Functor m => Functor (YesodClientMonadT cli m) where
    fmap f = YesodClientMonadT . fmap f . unYCT

instance (Functor m, Monad m) => Applicative (YesodClientMonadT cli m) where
    pure = YesodClientMonadT . pure
    (YesodClientMonadT f) <*> (YesodClientMonadT x) = YesodClientMonadT $ f <*> x


instance MonadTrans (YesodClientMonadT cli) where
    lift = YesodClientMonadT . lift . lift

instance Monad m => MonadReader cli (YesodClientMonadT cli m) where
    ask     = YesodClientMonadT . lift $ ask
    local f = YesodClientMonadT . StateT . fmap (local f) . runStateT . unYCT


instance Monad m => MonadState YesodClientState (YesodClientMonadT cli m) where
    state = YesodClientMonadT . state

instance MonadIO m => MonadIO (YesodClientMonadT cli m) where
    liftIO = YesodClientMonadT . liftIO

runYesodClientT                                :: YesodClientMonadT cli m a ->
                                                  cli ->
                                                  YesodClientState ->
                                                      m (a,YesodClientState)
runYesodClientT (YesodClientMonadT comp) cli s = runReaderT (runStateT comp s) cli



evalYesodClientT            :: Functor m => YesodClientMonadT cli m a ->
                                 cli -> YesodClientState -> m a
evalYesodClientT comp cli s = fst <$> runYesodClientT comp cli s





clientInstance :: Monad m => YesodClientMonadT cli m cli
clientInstance = ask



cookieJar :: (Monad m, Functor m) => YesodClientMonadT cli m (Maybe CookieJar)
cookieJar = cookieJar' <$> get


updateCookieJar   :: Monad m => Response body => YesodClientMonadT cli m ()
updateCookieJar r = modify $ \st -> st {cookieJar' = Just . responseCookieJar $ r}






type IsYesodClientFor client server = ( IsYesodClient client
                                      , Yesod server
                                      , YesodServer client ~ server
                                      )

-- type IsActionMonadFor m client = ( YesodClientAction m
--                                  , ClientInstance m ~ client
--                                  )

--------------------------------------------------------------------------------







toUrl     :: (client `IsYesodClientFor` server) =>
             client -> Route server -> String
toUrl cli r = let root     = serverAppRoot cli
                  (pcs,qs) = renderRoute r
                  urlBldr  = joinPath (server cli) root pcs qs in
              LC.unpack . toLazyByteString $ urlBldr



toReq   :: (client `IsYesodClientFor` server,
            Functor m,
            Failure HttpException m ) =>
           Route server -> YesodClientMonadT client m Request
toReq r = do
  cli <- clientInstance
  mcj <- cookieJar
  req <- parseUrl . toUrl cli $ r
  return $ req { HC.cookieJar       = mcj
               , HC.responseTimeout = Nothing
               }


runRouteWith   :: ( client `IsYesodClientFor` server
               , MonadResource m, MonadBaseControl IO m
               , Failure HttpException m
               ) =>
               Route server ->
               (Request -> Request) ->
                   YesodClientMonadT client m (Response (ResumableSource m ByteString))
runRouteWith r f = do
  mgr <- manager <$> clientInstance
  req' <- toReq r
  let req = f req'
  lift $ http req mgr

runGetRoute :: ( client `IsYesodClientFor` server
               , MonadResource m, MonadBaseControl IO m
               , Failure HttpException m
               ) =>
               Route server ->
                   YesodClientMonadT client m (Response (ResumableSource m ByteString))
runGetRoute = flip runRouteWith id


runPostRoute :: ( client `IsYesodClientFor` server
                , MonadResource m, MonadBaseControl IO m
                , Failure HttpException m) =>
                Route server -> Source (ResourceT IO) ByteString
                  -> YesodClientMonadT client m (Response (ResumableSource m ByteString))
runPostRoute r s = runRouteWith r $ \req ->
                   req { method      = methodPost
                       , requestBody = requestBodySourceChunked s
                       }



runDeleteRoute :: ( client `IsYesodClientFor` server
                  , MonadResource m, MonadBaseControl IO m
                  , Failure HttpException m) =>
                  Route server -> YesodClientMonadT client m (Response (ResumableSource m ByteString))
runDeleteRoute r = runRouteWith r $ \req ->
                     req { method      = methodDelete
                         , requestBody = RequestBodyBS empty
                         }


class ToBuilder a where
    toBuilder :: Monad m => Source m a -> Source m Builder


instance ToBuilder ByteString where
    toBuilder = mapOutput (\bs -> fromByteString bs <> flush)
