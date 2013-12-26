{-# Language  FlexibleContexts #-}
module HSync.Client.Actions where


import Control.Failure
import Control.Exception(throw)

import Control.Monad(when)
import Control.Monad.IO.Class (liftIO)


import Data.Aeson( Value
                 , fromJSON
                 ,  Result(..)
                 )
import Data.Aeson.Parser(value)

import Data.Conduit.Attoparsec(sinkParser , conduitParser)

import Data.ByteString(ByteString)
import Data.Conduit
import Data.Conduit.Binary
import Data.Conduit.Internal(ResumableSource(..))



import HSync.Client.Sync(Sync, user, hashedPassword, clientIdent)
import HSync.Client.ActionT
import HSync.Client.AcidActions(updateFileIdent)

import HSync.Common.DateTime(DateTime)
import HSync.Common.MTimeTree
import HSync.Common.HttpRequest(getHeader, hFileIdent)

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
                           , responseHeaders
                           )
import Network.HTTP.Types
import Network.Wai(requestBody)


import System.Directory( createDirectory
                       , removeFile , removeDirectoryRecursive )

import Yesod.Client


import qualified Data.ByteString.Char8 as B

import qualified Data.Attoparsec.Types      as AP
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Conduit               as C
import qualified Data.Conduit.List          as CL
import qualified Data.Text                  as T

import qualified HSync.Common.FileIdent     as FI

import Debug.Trace

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------

login :: Action Bool
login = do
  sync <- getSync
  resp <- runGetRoute $ MyLoginR (user sync) (hashedPassword sync)
  body <- lift $ responseBody resp C.$$+- sinkLbs
  case LB.unpack body of
    "VALID"   -> setSessionCreds resp >> return True
    "INVALID" -> return False


setSessionCreds :: Response body -> Action ()
setSessionCreds = liftYT . updateCookieJar

--------------------------------------------------------------------------------

-- | run the ListenR handler and get a source with all the
-- notifications. Filter out the ones that I triggered myself.
changes      :: DateTime -> Path -> Action (ResumableSource SyncBaseMonad Notification)
changes dt p = do
                 myCi <- clientIdent <$> getSync
                 filterChanges myCi <$> changes' dt p

    where
      isMyAction    myCi (Notification _ ci _) = ci == myCi
      filterChanges myCi (ResumableSource s final) =
          ResumableSource (s $= CL.filter (not . isMyAction myCi)) final


-- | run the ListenR handler and get a source of Notifications.
changes'      :: DateTime -> Path ->
                 Action (ResumableSource SyncBaseMonad Notification)
changes' dt p = do
                 resp <- runGetRoute $ ListenR dt p
                 return . toJSONSource . responseBody $ resp
    where
      toJSONSource (ResumableSource s final) =
          ResumableSource (CL.mapM_ (liftIO . print) $= s $= jsonConduit) final


-- | Transform the incoming bytestring stream representing a's in the form of
-- JSON into actual a's
jsonConduit :: (MonadThrow m, FromJSON a) => Conduit ByteString m a
jsonConduit = conduitParser jsonParser C.=$= CL.map snd


-- -- | Simply output the list of changes
printChanges   :: Path -> Action ()
printChanges p = do
  now <- liftIO $ currentTime
  cs  <- changes' now p
  lift $ cs C.$$+- printItem =$ CL.map (B.pack . show) =$ sinkFile "/tmp/notifications"
  where
    printItem = awaitForever $ \x -> liftIO (print x) >> yield x



--------------------------------------------------------------------------------

-- | Get the fileident and the path for the file with local path fp
remoteFileInfo    :: FilePath -> Action (FileIdent,Path)
remoteFileInfo fp = do
                      p  <- toRemotePath fp
                      fi <- toFileIdent <$> getRemoteTree p
                      liftIO $ print (p,fi)
                      return (fi,p)

-- | Runs the getTree Handler: get the FSTree representing the Filestystem at p
getRemoteTree   :: Path -> Action (Maybe MTimeFSTree)
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
jsonParser = value >>= \v -> case fromJSON v of
                               Error s   -> fail s
                               Success x -> return x
             -- Note: we are using the json value parser here, since we want to
             --       be able to parse 'null' values (in case of Maybe ).

--------------------------------------------------------------------------------

-- | Run a getFile handler to download the file with path p. The file is stored
-- at the `default' local path corresponding to p.
getFile   :: Path -> Action ()
getFile p = toLocalPath p >>= getFile' p


-- | run the getFile handler to download the file with path p. The contents of
-- that file are written to the (local) file with path lp
getFile'      :: Path -> FilePath -> Action ()
getFile' p lp = do
  resp <- runGetRoute $ FileR p
  lift (responseBody resp C.$$+- sinkFile lp)
  case (getHeader hFileIdent . responseHeaders $ resp) >>= fromPathPiece of
    Nothing -> throw $ InvalidHeader (B.pack "fileIdent header")
    Just fi -> updateFileIdent p fi


--------------------------------------------------------------------------------

-- | run putDir: i.e. create a new directory with path p
putDir   :: Path -> Action ()
putDir p = runPostRoute (PutDirR NonExistent p) noData >> return ()
    where
      noData = sourceLbs LB.empty
      -- TODO: Update the remote state

-- | Run a postPut action: i.e. upload the file at fp onto the server. We
-- assume that fp is a global path to the file!
putFile         :: FilePath -> FileIdent -> Path -> Action ()
putFile fp fi p = do
                     let h = PutFileR fi p
                         s = sourceFile fp
                     sync <- getSync
                     liftIO $ print (fp,fi,p)
                     liftIO $ print $ toUrl sync h
                     resp <- runPostRoute h s
                     liftIO $ print "woei"
                     -- TODO: Update the remote state


-- | Given a local (absolute) file path. Upload the file or directory.
putFileOrDir    :: FilePath -> Action ()
putFileOrDir fp = fileIdent fp >>= \fi -> case fi of
                    FI.NonExistent -> error "putFileOrDir: nonexistent file"
                    FI.Directory _ -> toRemotePath fp >>= putDir
                    FI.File      _ -> toRemotePath fp >>= putFile fp fi

-- | Force uploading the file at fp onto the server. I.e. get the remote file
-- info for this file, and then upload the file. Note that this is unsafe in
-- the sense that this will replace the file of the server without the
-- guarantee that our file is newer.
forcePutFile    :: FilePath -> Action ()
forcePutFile fp = remoteFileInfo fp >>= uncurry (putFile fp)

--------------------------------------------------------------------------------
-- | Updates

getUpdate      :: Path -> FileIdent -> Action ()
getUpdate p fi = getFile p


putUpdate :: FilePath -> FileIdent -> Path -> Action ()
putUpdate = putFile

--------------------------------------------------------------------------------
-- | Deletes

-- | Runs the DeleteR handler: i.e. deletes the file (with path p) on the
-- server, assuming that the remote file (still) has fileIdent fi.
deleteRemote      :: FileIdent -> Path -> Action ()
deleteRemote fi p = runDeleteRoute (DeleteR fi p) >> return ()
                    -- TODO: Update the state that we have of the remote server


--------------------------------------------------------------------------------
-- | Local actions

deleteFileLocally               :: Path -> FileIdent -> Action ()
deleteFileLocally _ NonExistent = error "deleteFileLocally: non existent file"
deleteFileLocally p fi          = toLocalPath p >>= liftIO . f
  where
    f = if FI.isDirectory fi then removeFile
                             else removeDirectoryRecursive


createDirectoryLocally   :: Path -> Action ()
createDirectoryLocally p = toLocalPath p >>= liftIO . createDirectory
