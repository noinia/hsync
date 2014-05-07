{-# Language  FlexibleContexts #-}
module HSync.Client.Actions where

import Prelude hiding (FilePath)

import Control.Exception(throw)

import Control.Monad(when)
import Control.Monad.Catch(MonadThrow(..))
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

import Data.Text.Encoding(encodeUtf8)

import Filesystem.Path.CurrentOS(FilePath, (<.>), encodeString, encode, decode)

import HSync.Client.Sync(Sync, user, password, clientIdent
                        , partialFileExtension
                        )
import HSync.Client.ActionT
import HSync.Client.Import
import HSync.Client.Logger
import HSync.Client.AcidActions( setByFileIdent
                               , deleteByFileIdent
                               , expectedFileIdent
                               )
import HSync.Client.TemporaryIgnored(withTemporarilyIgnored)


import HSync.Common.DateTime(DateTime, toEpochTime, currentTime)
import HSync.Common.TimedFSTree(MTimeTree)
import HSync.Common.Header
import HSync.Common.Notification(Notification(..))

-- import HSync.Server.Handler.Auth(requireRead,requireWrite)

import HSync.Server


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


import System.Directory( createDirectory, doesDirectoryExist
                       , removeFile , removeDirectoryRecursive
                       , renameFile )
import System.PosixCompat.Files(setFileTimes)

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
  infoM "Actions.login" "Sending login"
  resp <- runGetRoute $ MyLoginR (user sync) (password sync)
  body <- lift $ responseBody resp C.$$+- sinkLbs
  case LB.unpack body of
    "VALID"   -> noticeM    "Actions.login" "Login successful" >>
                 setSessionCreds resp                          >> return True
    "INVALID" -> criticalM "Actions.login" "Login failed!"     >> return False


setSessionCreds :: Response body -> Action ()
setSessionCreds = liftYT . updateCookieJar

--------------------------------------------------------------------------------

-- | run the ListenR handler and get a source with all the
-- notifications. Filter out the ones that I triggered myself.
changes      :: DateTime -> Path -> Action (ResumableSource SyncBaseMonad Notification)
changes dt p = do
                 myCi <- clientIdent <$> getSync
                 infoM "Actions.changes" msg
                 filterChanges myCi <$> changes' dt p

    where
      isMyAction    myCi (Notification _ ci _) = ci == myCi
      filterChanges myCi (ResumableSource s final) =
          ResumableSource (s $= CL.filter (not . isMyAction myCi)) final
      msg = "Getting Changes from" ++ show  dt ++ " on " ++ show p


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
getRemoteTree   :: Path -> Action (Maybe MTimeTree)
getRemoteTree p = do
                    infoM "Actions.getRemoteTree" ("Getting remote tree at: " ++ show p)
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
  infoM "Actions.getFile" ("Downloading " ++ show p ++ " to " ++ show lp)
  resp <- runGetRoute $ FileR p
  -- Download the file into a partial file
  let lpPartial = encodeString $ lp <.> partialFileExtension
  lift (responseBody resp C.$$+- sinkFile lpPartial)
  -- This file is incoming, so temporarily ignore listening for local changes
  -- of this file. (unignore in 1 second = 1e6 microseconds)
  withTemporarilyIgnored lp 1000000 $ do
    liftIO $ renameFile lpPartial (encodeString lp)
    -- set the modification time, and update the remote tree state
    withHeader HFileIdent resp (\fi ->    setModificationTime p fi
                                       >> setByFileIdent      p fi
                               )

--------------------------------------------------------------------------------

-- | run putDir: i.e. create a new directory with path p
putDir   :: Path -> Action ()
putDir p = infoM "Actions.putDir" ("Creating directory at " ++ show p) >>
           runPostRoute (PutDirR NonExistent p) noData >>= \resp ->
             withHeader HFileIdent resp (setByFileIdent p)
    where
      noData   = sourceLbs LB.empty

-- | Run a postPut action: i.e. upload the file at fp onto the server. We
-- assume that fp is a global path to the file!
putFile         :: FilePath -> FileIdent -> Path -> Action ()
putFile fp fi p = do
                     let h = PutFileR fi p
                         s = sourceFile $ encodeString fp
                     sync <- getSync
                     infoM "Actions.putFile" msg
                     debugM "Actions.putFile" ("url: " ++ toUrl sync h)
                     resp <- runPostRoute h s
                     -- let mh = headerValue HFileIdent . responseHeaders $ resp
                     --       ((getHeader hFileIdent . responseHeaders $ resp)
                     --          >>= fromPathPiece) :: Maybe FileIdent
                     withHeader HFileIdent resp (setByFileIdent p)
  where
    msg = concat [ "Uploading "
                 , show fp
                 , " with fileident '"
                 , show fi
                 , "' to '"
                 , show p
                 , "'"
                 ]

-- | Given a local (absolute) file path. Upload the file or directory.
putFileOrDir    :: FilePath -> Action ()
putFileOrDir fp = do
                    isDir <- liftIO $ doesDirectoryExist (encodeString fp)
                    p     <- toRemotePath fp
                    if isDir then putDir p
                             else expectedFileIdent p >>= \fi -> putFile fp fi p

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
deleteRemote fi p = infoM "Actions.deleteRemote" msg >>
                    runDeleteRoute (DeleteR fi p) >>= \resp ->
                      withHeader HDeletionTime resp (\dt ->
                         deleteByFileIdent p dt fi)
  where
    msg = "Deleting " ++ show p ++ " with FileIdent " ++ show fi
--------------------------------------------------------------------------------
-- | Local actions

deleteFileLocally               :: Path -> FileIdent -> Action ()
deleteFileLocally _ NonExistent = emergencyM "Actions.deleteFileLocally" "non existent file"
                                  >> error "deleteFileLocally: non existent file"
deleteFileLocally p fi          = infoM "Actions.deleteFileLocally" msg >>
                                  toLocalPath p >>= liftIO . f . encodeString
  where
    f = if FI.isDirectory fi then removeDirectoryRecursive
                             else removeFile
    msg = concat [ "Deleting local file with path ", show p
                 , " and FileIdent " , show fi ]



-- | Temporarily ignore a file or directory and then delete it.
deleteFileLocally'      :: Path -> FileIdent -> Action ()
deleteFileLocally' p fi = toLocalPath p >>= \lp ->
                          withTemporarilyIgnored lp 1000000 $
                            deleteFileLocally p fi


-- | Create a local directory
createDirectoryLocally   :: Path -> Action ()
createDirectoryLocally p = do
  infoM "Actions.createDirectoryLocally"
        ("Creating local directory for " ++ show p)
  lp <- toLocalPath p
  withTemporarilyIgnored (asDirectory lp) 1000000 $
    (liftIO . createDirectory . encodeString $ lp)

asDirectory    :: FilePath -> FilePath
asDirectory fp = decode $ encode fp <> "/"

--------------------------------------------------------------------------------
-- | Helper functions

-- | Run a computation with a given header
withHeader          :: IsTypedHeader h => h -> Response b
                    -> (HeaderValue h -> Action()) -> Action ()
withHeader h resp a = let n = encodeUtf8 $ headerName h in
                      maybe (throw $ InvalidHeader n) a
                    . headerValue h $ responseHeaders resp

setModificationTime      :: Path -> FileIdent -> Action ()
setModificationTime p fi = let t = toEpochTime . getDateTime $ fi in
  toLocalPath p >>= (\fp -> liftIO $ setFileTimes (encodeString fp) t t )
