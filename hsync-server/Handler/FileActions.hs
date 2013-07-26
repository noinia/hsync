module Handler.FileActions where

import Import
import Data.List(intercalate)

import System.Directory(removeFile)

import Handler.Auth(requireRead,requireWrite)

import Data.Conduit
import Data.Conduit.Binary
import Data.ByteString(ByteString)

import qualified Data.Text as T

-- TODO: Handle Directories!!!

dummy :: MonadResource m => Source m ByteString
dummy = sourceFile "/Users/frank/tmp/test.jpg"


serveSource s = respondSource typeOctet (s $= awaitForever sendChunkBS)


serveFile   :: MonadHandler m => Path -> m a
serveFile p = sendFile typeOctet (toFilePath p)


-- TODO
checkFileIdent     :: FileIdent -> Path -> Handler Bool
checkFileIdent _ _ = return True




toFilePath             :: Path -> FilePath
toFilePath (Path u ps) = intercalate "/" . map T.unpack $ filesDir:u:ps


postListenR             :: Path -> Handler Text
postListenR (Path u ps) = undefined


getFileR   :: Path -> Handler Text
getFileR p = protect (requireRead p) (serveFile p) (permissionDenied "file")

getDeltaR   :: Path -> Handler TypedContent
getDeltaR p = protect (requireRead p) serveDelta (permissionDenied "delta")
    where
      serveDelta = serveSource dummy


getSignatureR             :: Path -> Handler Text
getSignatureR (Path u ps) = undefined

deleteDeleteR                :: FileIdent -> Path -> Handler Text
deleteDeleteR fi p = protect (requireWrite p) delete' (permissionDenied "")
    where
      delete'  = protect (checkFileIdent fi p) doDelete (invalidArgs [])
               -- TODO: the error should really be a 409
      doDelete = (liftIO . removeFile . toFilePath $ p) >> return "OK"



postPatchR                :: FileIdent -> Path -> Handler Text
postPatchR fi (Path u ps) = undefined

postPutFileR             :: Path -> Handler Text
postPutFileR (Path u ps) = undefined

getChangesR                :: DateTime -> Path -> Handler Text
getChangesR dt (Path u ps) = undefined
