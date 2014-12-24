module HSync.Server.Handler.Auth where

import Control.Applicative
import Control.Monad(when)

import Data.Maybe
import Yesod
import Yesod.Auth
import HSync.Common.Header(lookupTypedHeader, HUserIdent(..), HPassword(..))

import HSync.Server.Import
import HSync.Server.LocalAuth(validateUser)
import HSync.Server.User(User(..))


--------------------------------------------------------------------------------

instance ToContent Bool where
  toContent = toContent . show

instance ToTypedContent Bool where
  toTypedContent = TypedContent typePlain . toContent


postMyLoginR :: Handler Bool
postMyLoginR = do
    mu@(Just u) <-                         lookupTypedHeader HUserIdent
    mpw         <- fmap hashedPassword <$> lookupTypedHeader HPassword
    b <- validateUser' mu mpw
    when b $ setCreds False $ Creds "PostLoginR" (unUI u) []
    return b
  where
    validateUser' mu mpw = fromMaybe (pure False) $ liftA2 validateUser mu mpw

--------------------------------------------------------------------------------
-- | permissions
