{-# LANGUAGE ConstraintKinds #-}
module HSync.Server.LocalAuth where

import Prelude

import Control.Applicative
import Control.Monad(when)

import Control.Monad.IO.Class(MonadIO(..))

import Data.Acid(EventState)

import HSync.Common.Types
import HSync.Common.Import(protect)

import HSync.Server.AcidState
import HSync.Server.AcidSync
import HSync.Server.User

import Yesod
import Yesod.Auth


import qualified Yesod.Auth.Message as Msg

--------------------------------------------------------------------------------

type AcidMonad m e = (Functor m, MonadIO m, HasAcidState m (EventState e))

-- | Given a (user,password) in plaintext, validate them against the
--   database values
validateUser       :: AcidMonad m LookupUser
                   => UserIdent -> HashedPassword -> m Bool
validateUser ui pw = maybe False checkPassword <$> queryAcid (LookupUser ui)
  where
    checkPassword u = pw == password u


authLocal :: AuthPlugin master
authLocal = AuthPlugin "local" dispatch login
  where
    dispatch "POST" ["login"] = postLoginR >>= sendResponse

    url = PluginR "local" []

    login authToMaster =
        toWidget [hamlet| "woei"
                  |]

postLoginR :: RenderMessage master FormMessage
           => HandlerT Auth (HandlerT master IO) TypedContent
postLoginR = do
    (u,hpw) <- lift $ runInputPost $ (,)
                        <$>                   ireq userIdField "username"
                        <*> (fmap hPassword $ ireq textField   "password")
    protect (validateUser u hpw)
            (validKey   u)
            invalidKey
  where
    hPassword = hashedPassword . Password

    validKey u = do
        lift $  setCredsRedirect $ Creds "localAuth" (unUI u) []
    msgIk = Msg.InvalidKey
    invalidKey = do
        mr      <- lift getMessageRender
        messageJson401 (mr msgIk) $ lift $ authLayout $ do
        setTitleI msgIk
          [whamlet|
$newline never
<p>_{msgIk}
|]



-- userForm = mkUser
--            <$> areq userIdField   "username" Nothing
--            <*> areq textField     "realname" Nothing
--            <*> areq passwordField "password" Nothing
--   where
--     mkUser i n p = User i (RealName n) (hashedPassword $ Password p)

--userIdField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m UserIdent

userIdField :: (RenderMessage (HandlerSite m) FormMessage, Monad m)
            => Field m UserIdent
userIdField = checkMMap (return . userIdent) unUI textField
