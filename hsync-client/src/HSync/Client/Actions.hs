{-# Language  FlexibleContexts #-}
module HSync.Client.Actions where


import Control.Failure

import Control.Monad(when)
import Control.Monad.IO.Class (liftIO)


import Data.Aeson( Value
                 , fromJSON
                 ,  Result(..)
                 )
import Data.Aeson.Parser(json)

import Data.Conduit.Attoparsec(sinkParser)

import Data.ByteString(ByteString)
import Data.Conduit
import Data.Conduit.Binary



import HSync.Client.Sync(Sync, user, hashedPassword)
import HSync.Client.ActionT
import HSync.Client.FSStatus

import HSync.Common.DateTime(DateTime)
import HSync.Common.FSTree

import HSync.Server.Import



import HSync.Server.Handler.Auth(requireRead,requireWrite)


import Network.HTTP.Conduit( Request
                           , Response
                           , Manager
                           , CookieJar
                           , RequestBody(..)
                           , HttpException(..)
                           , requestBody
                           , responseBody
                           , responseStatus
                           , responseCookieJar
                           )
import Network.HTTP.Types
import Network.Wai(requestBody)


import System.Directory( removeFile )

import Yesod.Client



import qualified Data.Conduit.List          as CL
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Conduit               as C
import qualified Network.HTTP.Conduit       as HC
import qualified Data.Text                  as T
import qualified HSync.Common.FileIdent     as FI



import Debug.Trace

--------------------------------------------------------------------------------

login :: ( MonadResource m, Failure HttpException m
         , MonadBaseControl IO m) => ActionT m Bool
login = do
  sync <- getSync
  resp <- runGetRoute $ MyLoginR (user sync) (hashedPassword sync)
  body <- lift $ responseBody resp C.$$+- sinkLbs
  case LB.unpack body of
    "VALID"   -> setSessionCreds resp >> return True
    "INVALID" -> return False


setSessionCreds :: Monad m => Response body -> ActionT m ()
setSessionCreds = updateCookieJar

--------------------------------------------------------------------------------

remoteFileInfo    :: ( MonadResource m, MonadThrow m, MonadIO m
                     , MonadBaseControl IO m, Failure HttpException m
                     ) => FilePath -> ActionT m (FileIdent,Path)
remoteFileInfo fp = do
                      p  <- toRemotePath fp
                      fi <- toFileIdent <$> getRemoteTree p
                      return (fi,p)


-- | Runs the getTree Handler
getRemoteTree   :: ( MonadResource m, MonadThrow m
                   , MonadBaseControl IO m, Failure HttpException m) =>
                   Path -> ActionT m (FSTree DateTime)
getRemoteTree p = do
                    liftIO $ print $ "path: " ++ show p
                    resp <- runGetRoute $ TreeR p
                    lift $ responseBody resp C.$$+- parseFromJSONSink



parseFromJSONSink :: (MonadThrow m, FromJSON a) => Sink ByteString m a
parseFromJSONSink = sinkParser jsonParser
    where
      jsonParser = json >>= \v -> case fromJSON v of
                                    Error s   -> fail s
                                    Success x -> return x

--------------------------------------------------------------------------------

-- | Run a getFile
getFile   :: ( MonadResource m, Failure HttpException m
             , MonadBaseControl IO m) => Path -> ActionT m ()
getFile p = do
  resp <- runGetRoute $ FileR p
  lp   <- toLocalPath p
  let status = responseStatus resp
  when (status == ok200) $ lift (responseBody resp C.$$+- sinkFile lp)

--------------------------------------------------------------------------------


putDir   :: ( MonadResource m, Failure HttpException m
            , MonadIO m, MonadBaseControl IO m) =>
            Path -> ActionT m ()
putDir p = runPostRoute (PutDirR NonExistent p) noData >> return ()
    where
      noData = sourceLbs LB.empty

-- | Run a postPut action
putFile    :: ( MonadResource m, Failure HttpException m
              , MonadIO m, MonadBaseControl IO m) => FilePath -> ActionT m ()
putFile fp = remoteFileInfo fp >>= uncurry (putFile' fp)


putFile'         :: ( MonadResource m, Failure HttpException m
                    , MonadIO m, MonadBaseControl IO m) =>
                    FilePath -> FileIdent -> Path -> ActionT m ()
putFile' fp fi p = do
                     let h = PutFileR fi p
                         s = sourceFile fp
                     sync <- getSync
                     liftIO $ print $ toUrl sync h
                     resp <- runPostRoute h s
                     liftIO $ print "woei"

--------------------------------------------------------------------------------
-- | Deletes


deleteFile      :: ( MonadResource m, Failure HttpException m
                   , MonadIO m, MonadBaseControl IO m) =>
                   FileIdent -> Path -> ActionT m ()
deleteFile fi p = runDeleteRoute (DeleteR fi p) >> return ()


deleteDir      :: ( MonadResource m, Failure HttpException m
                   , MonadIO m, MonadBaseControl IO m) =>
                   FileIdent -> Path -> ActionT m ()
deleteDir = deleteFile
