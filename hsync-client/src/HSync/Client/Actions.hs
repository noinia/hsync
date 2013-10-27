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

import Data.Conduit.Attoparsec(sinkParser , conduitParser)

import Data.ByteString(ByteString)
import Data.Conduit
import Data.Conduit.Binary
import Data.Conduit.Internal(ResumableSource(..))



import HSync.Client.Sync(Sync, user, hashedPassword)
import HSync.Client.ActionT

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


import qualified Data.Attoparsec.Types      as AP
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Conduit               as C
import qualified Data.Conduit.List          as CL
import qualified Data.Text                  as T


import Debug.Trace

--------------------------------------------------------------------------------


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

-- | run the ListenR handler and get a source of Notifications.
changes     :: ( MonadResource m, MonadThrow m
               , MonadBaseControl IO m, Failure HttpException m) =>
               DateTime -> Path -> ActionT m (ResumableSource m Notification)
changes dt p = do
                 resp <- runGetRoute $ ListenR dt p
                 return . toJSONSource . responseBody $ resp
    where
      toJSONSource (ResumableSource s final) =
          ResumableSource (s $= jsonConduit) final

-- | Transform the incoming bytestring stream representing a's in the form of
-- JSON into actual a's
jsonConduit :: (MonadThrow m, FromJSON a) => Conduit ByteString m a
jsonConduit = conduitParser jsonParser C.=$= CL.map snd

--------------------------------------------------------------------------------

-- | Get the fileident and the path for the file with local path fp
remoteFileInfo    :: ( MonadResource m, MonadThrow m, MonadIO m
                     , MonadBaseControl IO m, Failure HttpException m
                     ) => FilePath -> ActionT m (FileIdent,Path)
remoteFileInfo fp = do
                      p  <- toRemotePath fp
                      fi <- toFileIdent <$> getRemoteTree p
                      return (fi,p)

-- | Runs the getTree Handler: get the FSTree representing the Filestystem at p
getRemoteTree   :: ( MonadResource m, MonadThrow m
                   , MonadBaseControl IO m, Failure HttpException m) =>
                   Path -> ActionT m (FSTree DateTime)
getRemoteTree p = do
                    liftIO $ print $ "path: " ++ show p
                    resp <- runGetRoute $ TreeR p
                    lift $ responseBody resp C.$$+- parseFromJSONSink

-- | Parse the incoming bytestring stream into an a using the jsonParser
parseFromJSONSink :: (MonadThrow m, FromJSON a) => Sink ByteString m a
parseFromJSONSink = sinkParser jsonParser


-- | An Attoparsec parser that attempts to parse the incoming bytestring in
-- JSON format.
jsonParser :: FromJSON a => AP.Parser ByteString a
jsonParser = json >>= \v -> case fromJSON v of
                              Error s   -> fail s
                              Success x -> return x

--------------------------------------------------------------------------------

-- | Run a getFile handler to download the file with path p. The file is stored
-- at the `default' local path corresponding to p.
getFile   :: ( MonadResource m, Failure HttpException m
             , MonadBaseControl IO m) => Path -> ActionT m ()
getFile p = toLocalPath p >>= getFile' p


-- | run the getFile handler to download the file with path p. The contents of
-- that file are written to the (local) file with path lp
getFile'      :: ( MonadResource m, Failure HttpException m
                 , MonadBaseControl IO m) => Path -> FilePath -> ActionT m ()
getFile' p lp = do
  resp <- runGetRoute $ FileR p
  let status = responseStatus resp
  when (status == ok200) $ lift (responseBody resp C.$$+- sinkFile lp)

--------------------------------------------------------------------------------

-- | run putDir: i.e. create a new directory with path p
putDir   :: ( MonadResource m, Failure HttpException m
            , MonadIO m, MonadBaseControl IO m) =>
            Path -> ActionT m ()
putDir p = runPostRoute (PutDirR NonExistent p) noData >> return ()
    where
      noData = sourceLbs LB.empty

-- | Run a postPut action: i.e. upload the file at fp onto the server
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


-- | Runs the DeleteR handler: i.e. deletes the file (with path p) on the
-- server, assuming that the remote file (still) has fileIdent fi.
deleteFile      :: ( MonadResource m, Failure HttpException m
                   , MonadIO m, MonadBaseControl IO m) =>
                   FileIdent -> Path -> ActionT m ()
deleteFile fi p = runDeleteRoute (DeleteR fi p) >> return ()


-- runs the delete handler to delete the directory with path p (and FileIdent fi)
deleteDir      :: ( MonadResource m, Failure HttpException m
                   , MonadIO m, MonadBaseControl IO m) =>
                   FileIdent -> Path -> ActionT m ()
deleteDir = deleteFile
