module HSync.Server.Handler.FileActions where

import HSync.Server.Import

import Control.Concurrent.STM

import System.Directory( removeFile )

import HSync.Server.Handler.Auth(requireRead,requireWrite)

import Data.Conduit
import Data.Conduit.Binary
import Data.ByteString(ByteString)

import System.Lock.FLock

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
                   ns <- notifications <$> getYesod
                   c  <- liftIO $ atomically (dupTChan ns)
                   let evtSource = chanToSource c
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
deleteDeleteR fi p = atomicallyWriteR fi p "delete" delete'
    where
      delete' ci fp = protectedByFI fi fp "delete" $ do
                        removeFile fp
                        notification (FileRemoved p) ci

notification evt ci = currentTime >>= return . Notification evt ci


postPatchR                :: FileIdent -> Path -> Handler Text
postPatchR fi p = atomicallyWriteR fi p "patch" patch'
    where
      patch' ci = undefined

postPutFileR      :: FileIdent -> Path -> Handler Text
postPutFileR fi p = do
  wr <- waiRequest
  atomicallyWriteR fi p "file" (putFile' (requestBody wr))
    where
      putFile' s ci fp = protectedByFI fi fp "file" $ do
                           runResourceT $ s $$ sinkFile fp
                           notification (FileRemoved p) ci
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


logNotification   :: Notification -> Handler ()
logNotification n = do
                    c <- notifications <$> getYesod
                    lift $ atomically (writeTChan c n)

type FINotification = Either ErrorDescription Notification

atomicallyWriteR              :: FileIdent -> Path -> Text ->
                                 (ClientIdent -> FilePath -> IO FINotification) ->
                                     Handler Text
atomicallyWriteR fi p hName h = protectWrite p hName h'
    where
      fp = toFilePath filesDir p
      h' = do
             ci <- clientId
             mn <- liftIO $ atomicallyIO fp (h ci fp)
             case mn of
               Left err -> invalidArgs err
               Right n  -> logNotification n >> return "OK"


atomicallyIO    :: FilePath -> IO a -> IO a
atomicallyIO fp = withLock fp Exclusive Block


chanToSource   :: MonadIO m => TChan a -> Source m a
chanToSource c = do
                   x <- liftIO $ atomically (readTChan c)
                   yield x
                   chanToSource c

--------------------------------------------------------------------------------
-- | Testing functions


dummy :: MonadResource m => Source m ByteString
dummy = sourceFile "/Users/frank/tmp/test.jpg"
