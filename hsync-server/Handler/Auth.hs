{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TupleSections, TemplateHaskell, OverloadedStrings #-}
module Handler.Auth where

import Data.Maybe

import Import
import Yesod.Form

import Data.Text(Text)

import Data.Digest.Pure.SHA        (sha1, showDigest)

import qualified Data.Text                  as T
import qualified Data.ByteString.Lazy.Char8 as B


getMyLoginR      :: UserIdent -> HashedPassword -> Handler Text
getMyLoginR u hp = protect (validateUser u $ Just hp)
                           (do
                              setCreds False $ Creds "RESTfull" u []
                              return "VALID")
                           (return "INVALID")



--------------------------------------------------------------------------------
-- | Registration

hash :: Text -> Text
hash = T.pack . showDigest . sha1 . B.pack . T.unpack

userExists   :: UserIdent -> Handler Bool
userExists u = runDB (getBy . UniqueUser $ u) >>= return . isJust


postRegisterR :: Handler Html
postRegisterR = do
    ((result, _), _) <- runFormPost userForm
    case result of
        FormSuccess u -> tryInsert u
        _             -> invalidInput
    where
      tryInsert u@(User i _) = protect (not <$> userExists i)
                                           (do
                                             runDB $ insert u
                                             setMessage "User Registered."
                                             redirect HomeR)
                                           existingUser
      invalidInput = setMessage "Invalid Input" >> redirect RegisterR
      existingUser = setMessage "User exists"   >> redirect RegisterR





-- do
--   (mu,mp) <- lift $ runInputPost $ (,)
--              <$> iopt textField "username"
--              <*> iopt textField "password"
--
--   if isJust mu && isJust mp && not exists
--     then do
--
--            return "OK"
--     else do
--            setMessage "Invalid username/password"
--            redirect RegisterR


userForm :: Html -> MForm Handler (FormResult User, Widget)
userForm = renderDivs $ (\i p -> User i (Just $ hash p))
    <$> areq textField     "username" Nothing
    <*> areq passwordField "password" Nothing



getRegisterR :: Handler Html
getRegisterR = do
    -- Generate the form to be displayed
    (widget, enctype) <- generateFormPost userForm
    defaultLayout
        [whamlet|
            <form method=post action=@{RegisterR} enctype=#{enctype}>
                ^{widget}
                <p>It also doesn't include the submit button.
                <button>Submit
        |]

--------------------------------------------------------------------------------
-- | Permissions

requireRead             :: Path -> Handler Bool
requireRead (Path u ps) = do
                            ui <- requireAuthId'
                            return $ u == ui

requireWrite             :: Path -> Handler Bool
requireWrite p@(Path u ps) = requireRead p


requireAuthId' = maybeAuthId >>= maybe (permissionDenied "Login Required") return



-- | Given a (user,password) in plaintext, validate them against the
--   database values
validateUser              :: UserIdent -> Maybe HashedPassword -> Handler Bool
validateUser ui mHashedPw = runDB (getBy . UniqueUser $ ui) >>= \dbUser ->
    case dbUser of
        Nothing                 -> return False
        Just (Entity _ sqlUser) -> return $ mHashedPw == userPassword sqlUser
