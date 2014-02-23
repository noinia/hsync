module HSync.Client.LocalEvents where

import Control.Applicative((<$>))
import Control.Monad(when)
import Control.Monad.IO.Class(MonadIO(..))


import Data.Conduit(runResourceT)
import Data.Maybe
import Data.List(isSuffixOf)

import HSync.Client.ActionT
import HSync.Client.Actions
import HSync.Client.AcidActions
import HSync.Client.Logger
import HSync.Client.Sync(Sync(..), partialFileExtension)

import HSync.Common.Types
import HSync.Common.FileIdent(checkFileIdent)

import System.FilePath.GlobPattern((/~))


import System.FSNotify(watchTree, startManager, stopManager, eventPath)

import Filesystem.Path.CurrentOS(decodeString, encodeString)

import qualified System.FSNotify as FSN

-- whenFileHasChanged :: FP.FilePath -> (Path -> FilePath 0-> Action ()) -> Action ()
whenFileHasChanged fp' act = do
  p       <- toRemotePath fp
  act p fp
  -- changed <- fileHasChanged p
  -- if changed then act p fp
  --            else return ()
    where
      fp               = encodeString fp'
      fileHasChanged p = do
        eFi <- expectedFileIdent p


        r   <- checkFileIdent eFi fp
        debugM "LocalEvents.whenFileHasChanged" $ show r
        return $ isNothing r


-- | should we use the time?
handleEvent                 :: FSN.Event -> Action ()
handleEvent (FSN.Added fp' _)    = whenFileHasChanged fp' $ \_ fp -> do
                                    liftIO $ print "fileAdded "
                                    liftIO $ print fp
                                    putFileOrDir fp
handleEvent (FSN.Modified fp' _) = whenFileHasChanged fp' $ \p fp -> do
                                    liftIO $ print "fileModified "
                                    fi <- expectedFileIdent p
                                    liftIO $ print (fp,p,fi)
                                    putUpdate fp fi p
handleEvent (FSN.Removed fp' _)  = do
                                    p  <- toRemotePath (encodeString fp')
                                    fi <- expectedFileIdent p
                                    deleteRemote fi p
   -- TODO: Fix, the encodeString is a bit ugly. FSNotify uses system-filepath's
   -- FilePath data type.

actionPredicate :: Action (FSN.Event -> Bool)
actionPredicate = do
                    ignores <- ignore <$> getSync
                    let eventPath' = encodeString . eventPath
                    return $ \e -> and [ not $ eventPath' e `endsWith` partialFileExtension
                                       , all (eventPath' e /~) ignores
                                       ]


s `endsWith` x = x `isSuffixOf` s


syncUpstreamUntil           :: IO () -> Path -> Action ()
syncUpstreamUntil stopAct p = do
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
      stopAct
      stopManager mgr
