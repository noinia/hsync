module Handler.FileActions where

import Import
import Data.List(intercalate)

import qualified Data.Text as T


toFilePath             :: Path -> FilePath
toFilePath (Path u ps) = intercalate "/" . map T.unpack $ filesDir:u:ps




postRegisterClientR             :: Path -> Handler Text
postRegisterClientR (Path u ps) = undefined


getFileR             :: Path -> Handler Text
getFileR (Path u ps) = undefined

getDeltaR             :: Path -> Handler Text
getDeltaR (Path u ps) = undefined

getSignatureR             :: Path -> Handler Text
getSignatureR (Path u ps) = undefined

deleteDeleteR                :: FileIdent -> Path -> Handler Text
deleteDeleteR fi (Path u ps) = undefined

postPatchR                :: FileIdent -> Path -> Handler Text
postPatchR fi (Path u ps) = undefined

postPutFileR             :: Path -> Handler Text
postPutFileR (Path u ps) = undefined

getChangesR                :: DateTime -> Path -> Handler Text
getChangesR dt (Path u ps) = undefined
