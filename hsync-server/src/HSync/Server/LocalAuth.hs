{-# LANGUAGE ConstraintKinds #-}
module HSync.Server.LocalAuth where

import Prelude

import Control.Applicative
import Control.Monad(when)

import Control.Monad.IO.Class(MonadIO(..))

import Data.Acid(EventState)
import Data.Maybe
import Data.Monoid

import HSync.Common.Types
import HSync.Common.Import(protect)

import HSync.Server.AcidState
import HSync.Server.AcidSync
import HSync.Server.User
import HSync.Server.FileSystemState(newUserFSState)

import System.Directory(createDirectory)


import Yesod
import Yesod.Auth


import qualified Yesod.Auth.Message as Msg
import qualified Data.Text as T

--------------------------------------------------------------------------------
-- | The Acid actions that actually validate/insert stuff

type AcidMonad m e = (Functor m, MonadIO m, HasAcidState m (EventState e))

-- | Given a (user,password) in plaintext, validate them against the
--   database values
validateUser       :: AcidMonad m LookupUser
                   => UserIdent -> HashedPassword -> m Bool
validateUser ui pw = maybe False checkPassword <$> queryAcid (LookupUser ui)
  where
    checkPassword u = pw == password u


userExists   :: AcidMonad m LookupUser => UserIdent -> m Bool
userExists u = isJust <$> queryAcid (LookupUser u)




getFilesDir = return "/"

-- | Create the directory for this user, and update the FSState
createFilesDir   :: AcidMonad m NewUserDirectory => User -> m ()
createFilesDir u = let n       = T.unpack . unUI . userId $ u
                       a </> b = a <> "/" <> b
                    in
                    do
                      fd <- getFilesDir
                      let fp = fd </> n
                      liftIO $ createDirectory fp
                      newUserFSState fp
                        >>= updateAcid . NewUserDirectory (userId u)




--------------------------------------------------------------------------------
--  The Auth plugin














loginR, registerR :: AuthRoute
loginR    = PluginR "local" ["login"]
registerR = PluginR "local" ["register"]


localAuth :: ( AcidMonad (HandlerT master IO) LookupUser
             , YesodAuth master
             )
             => AuthPlugin master
localAuth = AuthPlugin "local" dispatch login
  where
    dispatch "POST" ["login"]    = postLoginR    >>= sendResponse
    dispatch "GET"  ["register"] = getRegisterR  >>= sendResponse
    dispatch "POST" ["register"] = postRegisterR >>= sendResponse
    dispatch _      _            = notFound


    url = PluginR "local" []

    login authToMaster =
        [whamlet|
$newline never
<div class="jumbotron">

<div class="container">

  <form role="form" method="post"
       action="@{authToMaster loginR}">

    <div class="form-group">
      <input type="text" name="userIdent" placeholder="Username" class="form-control">

    <div class="form-group">
      <input type="password" name="password" placeholder="Password" class="form-control">

    <button type="submit" class="btn btn-success">Sign in
                  |]



--------------------------------------------------------------------------------
-- | Login

postLoginR :: ( RenderMessage master FormMessage
              , YesodAuth master
              , AcidMonad (HandlerT master IO) LookupUser
              )
           => HandlerT Auth (HandlerT master IO) TypedContent
postLoginR = do
    (u,hpw) <- lift $ runInputPost $ (,)
                        <$>                   ireq userIdField "userIdent"
                        <*> (fmap hPassword $ ireq textField   "password")
    protect (lift $ validateUser u hpw)
            (validUser   u)
            invalidUser
  where
    hPassword = hashedPassword . Password

    validUser u = lift $  setCredsRedirect $ Creds "local" (unUI u) []
    invalidUser = loginErrorMessageI LoginR Msg.InvalidUsernamePass


--------------------------------------------------------------------------------
-- | Register

getRegisterR :: RenderMessage master FormMessage
             => HandlerT Auth (HandlerT master IO) TypedContent
getRegisterR = do
    -- Generate the form to be displayed
    (widget, enctype) <- lift $ generateFormPost registerForm
    undefined
    -- defaultLayout $(widgetFile "register")


postRegisterR :: ( RenderMessage master FormMessage
                 , YesodAuth master
                 , AcidMonad (HandlerT master IO) LookupUser
                 )
              => HandlerT Auth (HandlerT master IO) TypedContent
postRegisterR = do
    ((result, _), _) <- lift $ runFormPost registerForm
    case result of
        FormSuccess u -> tryInsert u
        _             -> invalidInput
    where
      tryInsert u = undefined
      invalidInput = undefined

      -- tryInsert u = updateAcid (InsertUser u)
      --                 >>= \me -> case me of
      --                       Nothing  -> createFilesDir u
      --                                     >> setMessage "User Registered."
      --                                     >> redirect HomeR
      --                       Just err -> setMessage (toHtml err)
      --                                     >> redirect RegisterR
      -- invalidInput = setMessage "Invalid Input" >> redirect RegisterR



registerForm = renderDivs $ do mkUser
                        <$> areq userIdField   "userIdent" Nothing
                        <*> areq textField     "realName"  Nothing
                        <*> areq passwordField "password"  Nothing
  where
    mkUser i n p = User i (RealName n) (hashedPassword $ Password p)


--------------------------------------------------------------------------------
-- | Helper stuff

userIdField :: (RenderMessage (HandlerSite m) FormMessage, Monad m)
            => Field m UserIdent
userIdField = checkMMap (return . userIdent) unUI textField
