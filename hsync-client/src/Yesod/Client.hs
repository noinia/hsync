{-# Language   TypeOperators
             , TypeFamilies
             , ConstraintKinds
             , Rank2Types
             , FlexibleContexts
  #-}
module Yesod.Client where

import Control.Applicative((<$>))

import Blaze.ByteString.Builder( Builder
                               , fromByteString
                               , flush
                               , toLazyByteString
                               )


import Control.Failure

import Data.Monoid((<>))
import Data.ByteString(ByteString)
import Data.Conduit(Source, ResumableSource,mapOutput)
import Data.Text(Text)


import Network.HTTP.Conduit( Request
                           , Response
                           , Manager
                           , CookieJar
                           , RequestBody(..)
                           , HttpException(..)
                           , http
                           , parseUrl
                           -- , createCookieJar
                           -- , withManager
                           , method
                           , requestBody
                           -- , responseBody
                           -- , responseCookieJar
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


class YesodClientAction m where
    type ClientInstance m

    clientInstance :: m (ClientInstance m)
    cookieJar      :: m (Maybe CookieJar)


type IsYesodClientFor client server = ( IsYesodClient client
                                      , Yesod server
                                      , YesodServer client ~ server
                                      )

type IsActionMonadFor m client = ( YesodClientAction m
                                 , ClientInstance m ~ client
                                 )

--------------------------------------------------------------------------------







toUrl     :: (client `IsYesodClientFor` server) =>
             client -> Route server -> String
toUrl cli r = let root     = serverAppRoot cli
                  (pcs,qs) = renderRoute r
                  urlBldr  = joinPath (server cli) root pcs qs in
              LC.unpack . toLazyByteString $ urlBldr



toReq   :: (client `IsYesodClientFor` server,
            m `IsActionMonadFor` client,
            Failure HttpException m ) =>
           Route server -> m (Request m')
toReq r = do
  cli <- clientInstance
  mcj <- cookieJar
  req <- parseUrl . toUrl cli $ r
  return $ req { HC.cookieJar = mcj }


runRouteWith   :: ( client `IsYesodClientFor` server
               , m `IsActionMonadFor` client
               , MonadResource m, MonadBaseControl IO m
               , Failure HttpException m
               ) =>
               Route server ->
               (Request m -> Request m) ->
                   m (Response (ResumableSource m ByteString))
runRouteWith r f = do
  mgr <- manager <$> clientInstance
  req' <- toReq r
  let req = f req'
  http req mgr

runGetRoute :: ( client `IsYesodClientFor` server
               , m `IsActionMonadFor` client
               , MonadResource m, MonadBaseControl IO m
               , Failure HttpException m
               ) =>
               Route server -> m (Response (ResumableSource m ByteString))
runGetRoute = flip runRouteWith id


runPostRoute :: ( client `IsYesodClientFor` server
                , m `IsActionMonadFor` client
                , ToBuilder a
                , MonadResource m, MonadBaseControl IO m
                , Failure HttpException m) =>
                Route server -> Source m a
                  -> m (Response (ResumableSource m ByteString))
runPostRoute r s = runRouteWith r $ \req ->
                   req { method      = methodPost
                       , requestBody = RequestBodySourceChunked . toBuilder $ s
                       }

class ToBuilder a where
    toBuilder :: Monad m => Source m a -> Source m Builder


instance ToBuilder ByteString where
    toBuilder = mapOutput (\bs -> fromByteString bs <> flush)
