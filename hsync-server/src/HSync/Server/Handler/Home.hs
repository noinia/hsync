{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module HSync.Server.Handler.Home where


import HSync.Server.Import
import HSync.Server.User
import HSync.Server.Handler.ViewTree(getViewTreeR)


-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler TypedContent
getHomeR = maybeAuthId >>= \mi -> case mi of
  Nothing -> toTypedContent <$> defaultLayout $(widgetFile "homepage")
  Just u  -> getViewTreeR (Path (userId u) [])


    -- (formWidget, formEnctype) <- generateFormPost sampleForm
    -- let submission = Nothing :: Maybe (FileInfo, Text)
    --     handlerName = "getHomeR" :: Text

-- postHomeR :: Handler Html
-- postHomeR = do
--     ((result, formWidget), formEnctype) <- runFormPost sampleForm
--     let handlerName = "postHomeR" :: Text
--         submission = case result of
--             FormSuccess res -> Just res
--             _ -> Nothing

--     defaultLayout $ do
--         aDomId <- newIdent
--         setTitle "Welcome To Yesod!"
--         $(widgetFile "homepage")

-- sampleForm :: Form (FileInfo, Text)
-- sampleForm = renderDivs $ (,)
--     <$> fileAFormReq "Choose a file"
--     <*> areq textField "What's on the file?" Nothing
