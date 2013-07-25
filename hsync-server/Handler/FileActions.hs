module Handler.FileActions where

import Import
import Data.List(intercalate)

import System.Directory(removeFile)

import Handler.Auth(requireRead,requireWrite)


import qualified Data.Text as T

-- TODO: Handle Directories!!!


serveFile   :: MonadHandler m => Path -> m a
serveFile p = sendFile typeOctet (toFilePath p)

checkFileIdent = undefined




toFilePath             :: Path -> FilePath
toFilePath (Path u ps) = intercalate "/" . map T.unpack $ filesDir:u:ps


postRegisterClientR             :: Path -> Handler Text
postRegisterClientR (Path u ps) = undefined


getFileR   :: Path -> Handler Text
getFileR p = protect (requireRead p) (serveFile p) (permissionDenied "")

getDeltaR             :: Path -> Handler Text
getDeltaR (Path u ps) = undefined

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
