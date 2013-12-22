module HSync.Server.Handler.FileActions where

import HSync.Server.Import


import Data.Aeson(encode)

import Data.Conduit
import Data.Conduit.Binary
import Data.ByteString(ByteString)


import Network.Wai(requestBody)

import HSync.Common.MTimeTree(MTimeFSTree, readMTimeTree)

import HSync.Server.Handler.Auth(requireRead,requireWrite)

import System.Directory( removeFile , createDirectory )


import qualified Data.Conduit.List as C
--import qualified Data.Text as T


--------------------------------------------------------------------------------
-- | Handles related to notifications

getListenR      :: DateTime -> Path -> Handler TypedContent
getListenR dt p = getListenNowR p
                  -- TODO:Fix this one, and merge it with the one with the one
                  -- below, since they are essencially the same


-- TODO: Filter the source so we only send the notifications matching pat h
getListenNowR   :: Path -> Handler TypedContent
getListenNowR p = protectRead p "listen" $ do
                   evtSource <- notifications
                   respondSource typePlain
                                 (evtSource $= C.map encode $= awaitForever sendChunk')
    where
      sendChunk' x = sendChunk x >> sendFlush

--------------------------------------------------------------------------------
-- | Handles related to listing files/trees

-- | Produces a JSON value representing a Maybe MTimeFSTree
getTreeR   :: Path -> Handler Value
getTreeR p = protectRead p "tree" $
               toJSON <$> getTreeOf p

getTreeOf   :: Path -> Handler (Maybe MTimeFSTree)
getTreeOf p = let fp = toFilePath filesDir p in
              liftIO $ protect (isPropperFile fp)
                               (readMTimeTree fp) -- TODO: Should I create a lock here?
                               (return Nothing)

--------------------------------------------------------------------------------
-- | Handles related to file events

getFileR   :: Path -> Handler TypedContent
getFileR p = protectRead p "file" $ serveFile p

getDeltaR   :: Path -> Handler TypedContent
getDeltaR p = protectRead p "delta" $ serveSource dummy

getSignatureR   :: Path -> Handler TypedContent
getSignatureR p = protectRead p "signature" $ serveSource dummy


deleteDeleteR                :: FileIdent -> Path -> Handler Text
deleteDeleteR fi p = atomicallyWriteR p "delete" delete'
    where
      delete' fp = protectedByFI fi fp "delete" $ do
                        liftIO $ removeFile fp
                        notification (FileRemoved p fi)

postPatchR                :: FileIdent -> Path -> Handler Text
postPatchR NonExistent _ = invalidArgs ["postPatch: cannot patch a nonexistent file."]
postPatchR fi          p = atomicallyWriteR p "patch" patch'
    where
      patch' = error "postPatch: unimplemented"

postPutFileR                 :: FileIdent -> Path -> Handler Text
postPutFileR (Directory _) _ = invalidArgs ["putFile: cannot replace directory by a file."]
postPutFileR fi            p = do
                                 bodySource' <- requestBody <$> waiRequest
                                 let bodySource = transPipe liftIO bodySource'
                                 -- transPipe lifts the underlying monad of
                                 -- bodySource' that is, the IO monad, into
                                 -- something more general; i.e. MonadIO m
                                 atomicallyWriteR p "putFile" $ putFile' bodySource
    where
      putFile' s fp = protectedByFI fi fp "putFile" $ do
                           liftIO . runResourceT $ s $$ sinkFile fp
                           notification (determineEvent fi p)

      determineEvent NonExistent = FileAdded
      determineEvent (File _)    = flip FileUpdated fi
      determineEvent _           = error "putFile: unknown event."




clientId :: Handler ClientIdent
clientId = return "clientId"








--------------------------------------------------------------------------------
-- | Handles related to directoryevents

postPutDirR     :: FileIdent -> Path -> Handler Text
postPutDirR fi p = withNotification putDir'
  --atomicallyWriteR p "putDir" putDir'
    where
      fp      = toFilePath filesDir p
      putDir' = protectedByFI fi fp "putDir" $ do
                        liftIO $ createDirectory fp
                        notification (DirectoryAdded p)


-- TODO: Handle Directories!!!

--------------------------------------------------------------------------------
-- | Helper functions


serveSource   :: Source Handler ByteString -> Handler TypedContent
serveSource s = respondSource typeOctet (s $= awaitForever sendChunkBS)


serveFile   :: MonadHandler m => Path -> m a
serveFile p = sendFile typeOctet (toFilePath filesDir p)

protectRead         :: Path -> Text -> Handler a -> Handler a
protectRead p err h = protect (requireRead p) h (permissionDenied err)

protectWrite         :: Path -> Text -> Handler a -> Handler a
protectWrite p err h = protect (requireWrite p) h (permissionDenied err)

type FINotification = Either ErrorDescription Notification

-- TODO: Use the hName somewhere
-- | Runs the given handler atomically.
atomicallyWriteR              :: Path -> Text ->
                                 (FilePath -> Handler FINotification) ->
                                     Handler Text
atomicallyWriteR p hName h = protectWrite p hName $ withNotification h'
    where
      fp = toFilePath filesDir p
      h' = atomicallyWriteIO fp (h fp)

-- | Runs a handler h that should proce a notification, and logs the
-- notification.
withNotification   :: Handler FINotification -> Handler Text
withNotification h = h >>= \mn -> case mn of
                       Left err -> invalidArgs err
                       Right n  -> logNotification n >> return "OK"


-- | Produce a notification with the given arguments and the current data/time
notification     :: EventKind -> Handler Notification
notification evt = Notification evt <$> clientId
                                    <*> liftIO currentTime

--------------------------------------------------------------------------------
-- | Testing functions


dummy :: MonadResource m => Source m ByteString
dummy = sourceFile "/Users/frank/tmp/test.jpg"
