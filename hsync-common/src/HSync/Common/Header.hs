{-# LANGUAGE TypeFamilies #-}
module HSync.Common.Header where

import Control.Applicative((<$>))
import Control.Monad((<=<))

import Data.ByteString(ByteString)
import Data.CaseInsensitive(mk)
import Data.Text
import Data.Text.Encoding(encodeUtf8,decodeUtf8)


import HSync.Common.DateTime(DateTime)
import HSync.Common.Types
import HSync.Common.FileIdent(FileIdent)

import Network.HTTP.Types.Header

import Yesod.Core(PathPiece(..), MonadHandler, addHeader, lookupHeader)

--------------------------------------------------------------------------------

class IsTypedHeader h where
  type HeaderValue h

  headerName        :: h -> Text

  parseHeaderValue  :: h -> ByteString    -> Maybe (HeaderValue h)
  encodeHeaderValue :: h -> HeaderValue h -> Text


asHeader     :: IsTypedHeader h => h -> HeaderValue h -> Header
asHeader h x = (headerName' h, encodeUtf8 $ encodeHeaderValue h x)


headerName' :: IsTypedHeader h => h -> HeaderName
headerName' = mkHeaderName . headerName

mkHeaderName :: Text -> HeaderName
mkHeaderName = mk . encodeUtf8


headerValue   :: IsTypedHeader h => h -> ResponseHeaders -> Maybe (HeaderValue h)
headerValue h = parseHeaderValue h <=< lookup (headerName' h)


lookupTypedHeader   :: (MonadHandler m, IsTypedHeader h) => h -> m (Maybe (HeaderValue h))
lookupTypedHeader h = (>>= parseHeaderValue h) <$> lookupHeader (headerName' h)


addTypedHeader   :: (MonadHandler m, IsTypedHeader h) => h -> HeaderValue h -> m ()
addTypedHeader h = addHeader (headerName h) . (encodeHeaderValue h)

--------------------------------------------------------------------------------

data HClientId = HClientId deriving (Show,Eq)


instance IsTypedHeader HClientId where
  type HeaderValue HClientId = ClientIdent

  headerName        _ = "clientId"
  parseHeaderValue  _ = Just . decodeUtf8
  encodeHeaderValue _ = id


------------------------------

data HFileIdent = HFileIdent deriving (Show,Eq)


instance IsTypedHeader HFileIdent where
  type HeaderValue HFileIdent = FileIdent

  headerName        _ = "fileIdent"
  parseHeaderValue  _ = fromPathPiece . decodeUtf8
  encodeHeaderValue _ = toPathPiece

------------------------------

data HHDeletionTime = HHDeletionTime deriving (Show,Eq)


instance IsTypedHeader HHDeletionTime where
  type HeaderValue HHDeletionTime = DateTime

  headerName        _ = "deletionTime"
  parseHeaderValue  _ = fromPathPiece . decodeUtf8
  encodeHeaderValue _ = toPathPiece
