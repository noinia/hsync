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
import HSync.Client.TemporaryIgnored(isTemporarilyIgnored)

import HSync.Common.Import(protect)
import HSync.Common.Types
import HSync.Common.FileIdent(checkFileIdent)

import System.FilePath.GlobPattern((/~))


import System.FSNotify(watchTree, startManager, stopManager, eventPath)

import Filesystem.Path.CurrentOS(decodeString, encodeString)

import qualified System.FSNotify as FSN

-- whenFileHasChanged :: FP.FilePath -> (Path -> FilePath 0-> Action ()) -> Action ()


whenNotIgnored fp' act =  do
  p       <- toRemotePath fp
  protect (isTemporarilyIgnored fp) (return ())
                                    (act p fp)
    where
      fp               = encodeString fp'
      fileHasChanged p = do
        eFi <- expectedFileIdent p


        r   <- checkFileIdent eFi fp
        debugM "LocalEvents.whenFileHasChanged" $ show r
        return $ isNothing r


-- | should we use the time?
handleEvent                 :: FSN.Event -> Action ()
handleEvent (FSN.Added fp' _)    = whenNotIgnored fp' $ \_ fp -> do
                                    liftIO $ print "fileAdded "
                                    liftIO $ print fp
                                    putFileOrDir fp
handleEvent (FSN.Modified fp' _) = whenNotIgnored fp' $ \p fp -> do
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
    s      <- getActionState
    r      <- getActionReader
    let handleEvent'   :: FSN.Event -> IO ()
        handleEvent' e = runResourceT $
                           runActionTWithYST
                             yState sync s r (handleEvent e)
    aPred  <- actionPredicate
    liftIO $ do
      mgr <- startManager
      watchTree mgr (decodeString fp) aPred handleEvent'
      stopAct
      stopManager mgr
