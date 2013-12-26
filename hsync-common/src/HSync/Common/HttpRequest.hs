module HSync.Common.HttpRequest where

import Data.CaseInsensitive(mk)

import Data.Text.Encoding(encodeUtf8,decodeUtf8)

import Network.HTTP.Types.Header

import Data.Text


mkHeaderName :: Text -> HeaderName
mkHeaderName = mk . encodeUtf8

mkRequestHeader     :: Text -> Text -> Header
mkRequestHeader n v = (mkHeaderName n, encodeUtf8 v)


-- | The HTTP-header identifying the client
hClientId :: HeaderName
hClientId = mkHeaderName "client-ident"

-- | The http-header identifying the file ident
hFileIdent :: HeaderName
hFileIdent = mkHeaderName hFileIdent'

hFileIdent' :: Text
hFileIdent' = "fileIdent"

getHeader   :: HeaderName -> ResponseHeaders -> Maybe Text
getHeader n = fmap decodeUtf8 . lookup n
