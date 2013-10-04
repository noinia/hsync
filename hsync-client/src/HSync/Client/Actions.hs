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


downloadFile :: ( MonadResource m, Failure HttpException m
                , MonadBaseControl IO m) =>
               (FileIdent, SubPath) -> ActionT m ()
downloadFile (_,sp) = toRemotePath' sp >>= getFile


-- downloadFile :: ( MonadResource m, Failure HttpException m
--                 , MonadBaseControl IO m) =>
--                (Change FileIdent, SubPath) -> ActionT m ()
patchFile = undefined

--------------------------------------------------------------------------------





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


uploadFile :: ( MonadResource m, Failure HttpException m
              , MonadIO m, MonadBaseControl IO m) =>
             (FileIdent, SubPath) -> ActionT m ()
uploadFile (fi,sp) = do
                       rp <- toRemotePath' sp
                       lp <- toLocalPath'  sp
                       putFile' lp fi rp

--------------------------------------------------------------------------------
-- | Deletes

deleteFile :: MonadIO m => (FileIdent, SubPath) -> ActionT m ()
deleteFile _ = return () -- TODO, fix this

--------------------------------------------------------------------------------

-- handleConflict                         :: Monad m =>
--                                           (Conflict FileIdent,SubPath) -> ActionT m ()
-- handleConflict ((Conflict loc rem),sp) = return () -- TODO

handleConflict _ = return ()

--------------------------------------------------------------------------------


syncTree               :: ( MonadResource m, Failure HttpException m
                          , MonadIO m, MonadBaseControl IO m) =>
                          Path -> ActionT m ()
syncTree p@(Path _ sp) = do
  Just oldRemote <- subTree sp <$> remoteTree -- TODO: fix the just
  newRemote <- getRemoteTree p
  newLocal  <- readFSTree =<< toLocalPath p
  handleChanges $ detectFSChanges oldRemote newRemote newLocal


runTree   :: (Monad m, Functor m) => ((l,SubPath) -> m b) -> FSTree l -> m (FSTree b)
runTree f = runBottomUp f . labelWithSubPaths


handleChanges         :: ( MonadResource m, Failure HttpException m
                         , MonadIO m, MonadBaseControl IO m) =>
                        FSChanges -> ActionT m ()
handleChanges changes = do
  runTree handleConflict . toHandleConflicts $ changes
  runTree deleteFile     . toDeleteLocal     $ changes
  runTree downloadFile   . toDownload        $ changes
  runTree patchFile      . toPatchLocal      $ changes
  return ()
