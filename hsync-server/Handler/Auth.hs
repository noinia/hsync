{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TupleSections, TemplateHaskell, OverloadedStrings #-}
module Handler.Auth where

import Data.Maybe

import Import
import Data.Text(Text)

import Data.ByteString.Lazy.Char8  (pack)
import Data.Digest.Pure.SHA        (sha1, showDigest)

import qualified  Data.Text as T


getMyLoginR      :: UserIdent -> HashedPassword -> Handler Text
getMyLoginR u hp = protect (validateUser u $ Just hp)
                           (do
                              setCreds True $ Creds "RESTfull" u []
                              return "VALID")
                           (return "INVALID")

-- getMyLoginR u = return $ RepPlain "login"


getTestR :: Handler Text
getTestR = do
  maid <- maybeAuthId
  return $ maybe "not logged in " (T.pack . show) maid




requireRead             :: Path -> Handler Bool
requireRead (Path u ps) = do
                            ui <- requireAuthId'
                            return $ u == ui

requireWrite             :: Path -> Handler Bool
requireWrite p@(Path u ps) = requireRead p


requireAuthId' = maybeAuthId >>= maybe (permissionDenied "Login Required") return

--------------------------------------------------------------------------------
-- Based on Yesod.Auth.HashDB



-- | Computer the sha1 of a string and return it as a string
-- sha1String :: Text -> String
-- sha1String = showDigest . sha1 . pack . T.unpack





-- | Given a (user,password) in plaintext, validate them against the
--   database values

validateUser              :: UserIdent -> Maybe HashedPassword -> Handler Bool
validateUser ui mHashedPw = runDB (getBy . UniqueUser $ ui) >>= \dbUser ->
    case dbUser of
        -- user not found
        Nothing                 -> return False
        -- validate password
        Just (Entity _ sqlUser) -> return $ mHashedPw == userPassword sqlUser
