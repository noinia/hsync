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





-- instanceState      :: Sync -> InstanceState Sync
-- instanceState inst = InstanceState { clientInstance = inst
--                                    , cookieJar      = createCookieJar []
--                                    }



-- | maintain mutable state and imutable state



-- runInstance ci = do
--                    loadPersistent
--                    login
--                    from <- return "now" -- TODO get the data from loadPersistent
--                    forkIO $ listenRemote ci from
--                    forkIO $ listenLocal ci


-- loadPersistent = return ()

-- listenRemote = undefined
-- listenLocal = undefined





--------------------------------------------------------------------------------





main :: IO ()
main = withSocketsDo $ withManager $ \mgr -> do
         let sync = def { httpManager    = mgr
                        , user           = myUser
                        , hashedPassword = myHashedPass
                        }
         x <- flip runActionT sync $ do
                loggedIn <- login
                when loggedIn $ putFile "/Users/frank/tmp/synced/test_put.jpg"
                -- when loggedIn $ getFile $ Path (user sync) ["test.jpg"]
                return loggedIn
         liftIO $ print x

--          case parseUrl urlString of
--            Nothing  -> liftIO $ putStrLn "Invalid URL"
--            Just req -> do
--                          let reqHead = req { method = "GET" }
--                          resp <- http reqHead manager
--                          liftIO $ print $ responseStatus resp
--                          liftIO $ mapM_ print $ responseHeaders resp
--                          responseBody resp C.$$+- sinkFile "google.html"
