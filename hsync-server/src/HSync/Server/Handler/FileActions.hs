module HSync.Server.Handler.FileActions where

import HSync.Server.Import

import System.Directory( removeFile )

import HSync.Server.Handler.Auth(requireRead,requireWrite)

import Data.Conduit
import Data.Conduit.Binary
import Data.ByteString(ByteString)


import Network.Wai(requestBody)


import qualified Data.Conduit.List as C
import qualified Data.Text as T



--------------------------------------------------------------------------------
-- | Handles related to notifications

getListenR      :: DateTime -> Path -> Handler Text
getListenR dt p = undefined
                  -- TODO: merge this one with the one below, since they are
                  -- essencially the same


-- TODO: Filter the source so we only send the notifications matching pat h
getListenNowR   :: Path -> Handler TypedContent
getListenNowR p = protectRead p "listen" $ do
                   evtSource <- notifications
                   respondSource typePlain
                                 (evtSource $= C.map show $= awaitForever sendChunk)




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
      delete' ci fp = protectedByFI fi fp "delete" $ do
                        removeFile fp
                        notification (FileRemoved p) ci

postPatchR                :: FileIdent -> Path -> Handler Text
postPatchR NonExistent _ = invalidArgs ["postPatch: cannot patch a nonexistent file."]
postPatchR fi          p = atomicallyWriteR p "patch" patch'
    where
      patch' ci = undefined

postPutFileR             :: FileIdent -> Path -> Handler Text
postPutFileR Directory _ = invalidArgs ["putFile: cannot replace directory by file."]
postPutFileR fi        p = do
                             wr <- waiRequest
                             atomicallyWriteR p "putFile" (putFile' (requestBody wr))
    where
      putFile' s ci fp = protectedByFI fi fp "putFile" $ do
                           runResourceT $ s $$ sinkFile fp
                           notification (determineEvent fi p) ci
      determineEvent NonExistent  = FileAdded
      determineEvent (FileHash _) = FileUpdated
      determineEvent (FileDate _) = FileUpdated
      determineEvent _            = error "putFile: unknown event."


    -- where
    --   putFile' = do
    --                ci <- clientId
    --                mn <- liftIO $ atomicallyIO fp (putFile'' ci)

clientId :: Handler ClientIdent
clientId = return "clientId"


--------------------------------------------------------------------------------
-- | Handles related to directoryevents

-- TODO: Handle Directories!!!

--------------------------------------------------------------------------------
-- | Helper functions


serveSource   :: Source Handler ByteString -> Handler TypedContent
serveSource s = respondSource typeOctet (s $= awaitForever sendChunkBS)


serveFile   :: MonadHandler m => Path -> m a
serveFile p = sendFile typeOctet (toFilePath filesDir p)


protectedByFI               :: MonadIO m => FileIdent -> FilePath -> Text -> m a ->
                               m (Either ErrorDescription a)
protectedByFI fi fp hName h = do
  me <- checkFileIdent fi fp
  case me of
    Nothing -> h >>= return . Right
    Just e  -> return . Left . insertHName hName $ e


insertHName   :: Text -> [Text] -> [Text]
insertHName n = map ((n <> ": ") <>)

protectRead         :: Path -> Text -> Handler a -> Handler a
protectRead p err h = protect (requireRead p) h (permissionDenied err)

protectWrite         :: Path -> Text -> Handler a -> Handler a
protectWrite p err h = protect (requireWrite p) h (permissionDenied err)

type FINotification = Either ErrorDescription Notification

atomicallyWriteR              :: Path -> Text ->
                                 (ClientIdent -> FilePath -> IO FINotification) ->
                                     Handler Text
atomicallyWriteR p hName h = protectWrite p hName h'
    where
      fp = toFilePath filesDir p
      h' = do
             ci <- clientId
             mn <- liftIO $ atomicallyWriteIO fp (h ci fp)
             case mn of
               Left err -> invalidArgs err
               Right n  -> logNotification n >> return "OK"


notification        :: EventKind -> ClientIdent -> IO Notification
notification evt ci = currentTime >>= return . Notification evt ci


--------------------------------------------------------------------------------
-- | Testing functions


dummy :: MonadResource m => Source m ByteString
dummy = sourceFile "/Users/frank/tmp/test.jpg"
