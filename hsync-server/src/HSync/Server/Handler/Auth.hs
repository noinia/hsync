module HSync.Server.Handler.Auth where

import Data.Maybe

import HSync.Server.Import
import HSync.Server.AcidSync
import HSync.Server.AcidState

import HSync.Server.User(User(..),RealName(..))

import System.Directory(createDirectory)

import qualified Data.Text as T

--------------------------------------------------------------------------------

getMyLoginR      :: UserIdent -> HashedPassword -> Handler Text
getMyLoginR u hp = protect (validateUser u hp)
                           (do
                              setCreds False $ Creds "RESTfull" (unUI u) []
                              return "VALID")
                           (return "INVALID")



--------------------------------------------------------------------------------
-- | Registration

userExists   :: UserIdent -> Handler Bool
userExists u = isJust <$> queryAcid (LookupUser u)

postRegisterR :: Handler Html
postRegisterR = do
    ((result, _), _) <- runFormPost userForm
    case result of
        FormSuccess u -> tryInsert u
        _             -> invalidInput
    where
      tryInsert u = updateAcid (InsertUser u)
                      >>= \me -> case me of
                            Nothing  -> createFilesDir u
                                          >> setMessage "User Registered."
                                          >> redirect HomeR
                            Just err -> setMessage (toHtml err)
                                          >> redirect RegisterR
      invalidInput = setMessage "Invalid Input" >> redirect RegisterR

-- | Create the directory for this user
createFilesDir   :: User -> Handler ()
createFilesDir u = let n       = T.unpack . unUI . userId $ u
                       a </> b = a <> "/" <> b
                    in
                    getFilesDir >>= liftIO . createDirectory . (</> n)


userForm :: Html -> MForm Handler (FormResult User, Widget)
userForm = renderDivs $ do mkUser
                        <$> areq userIdField   "username" Nothing
                        <*> areq textField     "realname" Nothing
                        <*> areq passwordField "password" Nothing
  where
    mkUser i n p = User i (RealName n) (hashedPassword $ Password p)

userIdField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m UserIdent
userIdField = checkMMap (return . userIdent) unUI textField

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

requireRead            :: Path -> Handler Bool
requireRead (Path u _) = (u ==) <$> requireAuthId'

-- | For now require the same thing as reading
requireWrite :: Path -> Handler Bool
requireWrite = requireRead


requireAuthId' :: Handler UserIdent
requireAuthId' = maybeAuthId >>= maybe (permissionDenied "Login Required") return



-- | Given a (user,password) in plaintext, validate them against the
--   database values
validateUser       :: UserIdent -> HashedPassword -> Handler Bool
validateUser ui pw = maybe False checkPassword <$> queryAcid (LookupUser ui)
  where
    checkPassword u = pw == password u


  -- runDB (getBy . UniqueUser $ ui) >>= \dbUser ->
  --   case dbUser of
  --       Nothing                 -> return False
  --       Just (Entity _ sqlUser) -> return $ mHashedPw == userPassword sqlUser
