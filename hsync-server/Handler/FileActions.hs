module Handler.FileActions where

import Import

import Control.Concurrent.STM

import System.Directory(removeFile)

import Handler.Auth(requireRead,requireWrite)

import Data.Conduit
import Data.Conduit.Binary
import Data.ByteString(ByteString)

import System.Lock.FLock


import qualified Data.Conduit.List as C
import qualified Data.Text as T



--------------------------------------------------------------------------------
-- | Handles related to notifications

getChangesR                :: DateTime -> Path -> Handler Text
getChangesR dt (Path u ps) = undefined


getListenR   :: Path -> Handler TypedContent
getListenR p = do
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
deleteDeleteR fi p = protectWrite p "delete" delete'
    where
      fp          = toFilePath filesDir p
      delete'     = do
                      ci <- return "clientId" -- TODO
                      mn <- liftIO $ atomicallyIO fp (delete'' ci)
                      case mn of
                        Nothing   -> invalidArgs [] -- TODO: this hosuld be a 409
                        (Just n)  -> logNotification n >> return "OK"
      delete'' ci = protectedByFI fi fp $ do
                      removeFile fp
                      t <- return "getCurrentTime" -- TODO!!!
                      return $ Notification (FileRemoved p) ci t

protectedByFI fi fp h = protect (checkFileIdent fi fp)
                                (h >>= return . Just)
                                (return Nothing)

postPatchR                :: FileIdent -> Path -> Handler Text
postPatchR fi (Path u ps) = undefined

postPutFileR             :: Path -> Handler Text
postPutFileR (Path u ps) = undefined


--------------------------------------------------------------------------------
-- | Handles related to directoryevents

-- TODO: Handle Directories!!!

--------------------------------------------------------------------------------
-- | Helper functions


serveSource   :: Source Handler ByteString -> Handler TypedContent
serveSource s = respondSource typeOctet (s $= awaitForever sendChunkBS)


serveFile   :: MonadHandler m => Path -> m a
serveFile p = sendFile typeOctet (toFilePath filesDir p)


-- TODO
checkFileIdent     :: Monad m => FileIdent -> FilePath -> m Bool
checkFileIdent _ _ = return True


protectRead         :: Path -> Text -> Handler a -> Handler a
protectRead p err h = protect (requireRead p) h (permissionDenied err)

protectWrite         :: Path -> Text -> Handler a -> Handler a
protectWrite p err h = protect (requireWrite p) h (permissionDenied err)


logNotification   :: Notification -> Handler ()
logNotification n = do
                    c <- notifications <$> getYesod
                    lift $ atomically (writeTChan c n)


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
