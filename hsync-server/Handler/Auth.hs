{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TupleSections, TemplateHaskell, OverloadedStrings #-}
module Handler.Auth where

import Data.Maybe

import Import

import Data.ByteString.Lazy.Char8  (pack)
import Data.Digest.Pure.SHA        (sha1, showDigest)

import qualified  Data.Text as T


getMyLoginR      :: UserIdent -> HashedPassword -> Handler Text
getMyLoginR u hp = validateUser (u,Just hp) >>= \isValid ->
                             if isValid
                             then do
                               setCreds True $ Creds "RESTfull" u []
                               return "VALID"
                             else return "INVALID"






-- getMyLoginR u = return $ RepPlain "login"


getTestR :: Handler Text
getTestR = do
  maid <- maybeAuthId
  return $ maybe "not logged in " (T.pack . show) maid




--------------------------------------------------------------------------------
-- Direclty copied from Yesod.Auth.HashDB



-- | Computer the sha1 of a string and return it as a string
-- sha1String :: Text -> String
-- sha1String = showDigest . sha1 . pack . T.unpack





-- | Given a (user,password) in plaintext, validate them against the
--   database values

-- validateUser :: (YesodPersist y,
--                  PersistBackend (YesodDB y (GGHandler sub y IO)))
--              => (Text, Text)
--              -> GHandler sub y Bool

validateUser (user,mHashedPw) = runDB (getBy $ UniqueUser user) >>= \dbUser ->
    case dbUser of
        -- user not found
        Nothing          -> return False
        -- validate password
        Just (Entity _ sqlUser) -> return $ mHashedPw == userPassword sqlUser
