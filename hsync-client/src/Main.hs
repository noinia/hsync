module Main where





import Creds


import Data.Default(def)


import HSync.Client.Actions
import HSync.Client.Import
import HSync.Client.Sync


import Control.Concurrent(forkIO)
import Control.Failure
import Control.Monad(when)

import Control.Monad.IO.Class (liftIO)


import Network.HTTP.Conduit( withManager )





import Network

import System.Environment (getArgs)

--------------------------------------------------------------------------------

type GlobalSettings = Text
type InstanceSettings = Text

--------------------------------------------------------------------------------

-- | The global application
data HSyncClient = HSyncClient { globalSettings :: GlobalSettings
                               }



--------------------------------------------------------------------------------





main :: IO ()
main = withSocketsDo $ withManager $ \mgr -> do
         let sync = def { httpManager    = mgr
                        , user           = myUser
                        , hashedPassword = myHashedPass
                        }
         x <- flip runActionT sync $ do
                loggedIn <- login
                -- when loggedIn $ putFile "/Users/frank/tmp/synced/test_put.jpg"
                -- when loggedIn $ getFile $ Path (user sync) ["test.jpg"]
                t <- getTree $ Path (user sync) []
                return t
         liftIO $ print x

--          case parseUrl urlString of
--            Nothing  -> liftIO $ putStrLn "Invalid URL"
--            Just req -> do
--                          let reqHead = req { method = "GET" }
--                          resp <- http reqHead manager
--                          liftIO $ print $ responseStatus resp
--                          liftIO $ mapM_ print $ responseHeaders resp
--                          responseBody resp C.$$+- sinkFile "google.html"
