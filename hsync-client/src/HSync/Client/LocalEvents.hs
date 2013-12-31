module HSync.Client.LocalEvents where

import Control.Applicative((<$>))
import Control.Monad.IO.Class(MonadIO(..))

import Data.Conduit(runResourceT)

import HSync.Client.ActionT
import HSync.Client.Actions
import HSync.Client.AcidActions
import HSync.Client.Sync(Sync(..))

import HSync.Common.Types

import System.FilePath.GlobPattern((/~))
import System.FSNotify(watchTree, startManager, stopManager, eventPath)

import Filesystem.Path.CurrentOS(decodeString, encodeString)


import qualified System.FSNotify as FSN

-- | should we use the time?
handleEvent                 :: FSN.Event -> Action ()
handleEvent (FSN.Added fp _)    = do
                                    liftIO $ print "fileAdded "
                                    liftIO $ print fp
                                    putFileOrDir (encodeString fp)
handleEvent (FSN.Modified fp _) = do
                                    p  <- toRemotePath (encodeString fp)
                                    fi <- serverFileState $ subPath p
                                    liftIO $ print "fileModified "
                                    liftIO $ print (fp,p,fi)
                                    putUpdate (encodeString fp) fi p
handleEvent (FSN.Removed fp _)  = do
                                    p  <- toRemotePath (encodeString fp)
                                    fi <- serverFileState $ subPath p
                                    deleteRemote fi p
   -- TODO: Fix, the encodeString is a bit ugly. FSNotify uses system-filepath's
   -- FilePath data type.

actionPredicate :: Action (FSN.Event -> Bool)
actionPredicate = do
                    ignores <- ignore <$> getSync
                    let eventPath' = encodeString . eventPath
                    return $ \e -> all (eventPath' e /~) ignores

syncUpstream   :: Path -> Action ()
syncUpstream p = do
                   sync   <- getSync
                   fp     <- toLocalPath p
                   yState <- getYesodClientState
                   acid   <- getAcidSync
                   let handleEvent'   :: FSN.Event -> IO ()
                       handleEvent' e = runResourceT $
                                          runActionTWithClientState
                                             yState sync acid (handleEvent e)
                   aPred  <- actionPredicate
                   liftIO $ do
                     mgr <- startManager
                     watchTree mgr (decodeString fp) aPred handleEvent'
                     print "press retrun to stop"
                     getLine
                     stopManager mgr
