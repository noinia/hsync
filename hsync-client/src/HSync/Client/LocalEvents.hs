module HSync.Client.LocalEvents where


import Prelude hiding (FilePath)

import Control.Applicative((<$>))
import Control.Monad(when)
import Control.Monad.IO.Class(MonadIO(..))


import Control.Monad.Trans.Resource(runResourceT)

import Data.Monoid
import Data.Maybe
import Data.List(isSuffixOf)

import Filesystem.Path.CurrentOS( FilePath, (<.>), (</>))

import HSync.Client.ActionT
import HSync.Client.Actions
import HSync.Client.AcidActions
import HSync.Client.Logger
import HSync.Client.Sync(Sync(..), isPartialFile)
import HSync.Client.TemporaryIgnored(isTemporarilyIgnored)

import HSync.Common.Import(protect)
import HSync.Common.Types
import HSync.Common.FileIdent(checkFileIdent)

import System.FilePath.GlobPattern(GlobPattern, (/~))


import System.FSNotify(watchTree, startManager, stopManager, eventPath)

import Filesystem.Path.CurrentOS(decodeString, encodeString)

import qualified System.FSNotify as FSN

-- whenFileHasChanged :: FP.FilePath -> (Path -> FilePath 0-> Action ()) -> Action ()


whenNotIgnored        :: FilePath -> (Path -> Action ()) -> Action ()
whenNotIgnored fp act =  do
  p       <- toRemotePath fp
  protect (isTemporarilyIgnored fp) (return ())
                                    (act p)
    where
      -- fp               = encodeString fp'
      fileHasChanged p = do
        eFi <- expectedFileIdent p


        r   <- checkFileIdent eFi (encodeString fp)
        debugM "LocalEvents.whenFileHasChanged" $ show r
        return $ isNothing r


-- | should we use the time?
handleEvent                 :: FSN.Event -> Action ()
handleEvent (FSN.Added fp _)    = whenNotIgnored fp $ \_ -> do
      debugM "LocalEvents.handleEvent" $ "File Added " <> show fp
      putFileOrDir fp
handleEvent (FSN.Modified fp _) = whenNotIgnored fp $ \p -> do
      debugM "LocalEvents.handleEvent" $ "File Modified " <> show fp
      fi <- expectedFileIdent p
      debugM "LocalEvents.handleEvent" $ show (fp,p,fi)
      putUpdate fp fi p
handleEvent (FSN.Removed fp _)  = do
      debugM "LocalEvents.handleEvent" $ "File Removed " <> show fp
      p  <- toRemotePath fp
      fi <- expectedFileIdent p
      deleteRemote fi p
   -- TODO: Fix, the encodeString is a bit ugly. FSNotify uses system-filepath's
   -- FilePath data type.

actionPredicate :: Action (FSN.Event -> Bool)
actionPredicate = do
                    ignores <- ignore <$> getSync
                    return $ \e -> and [ not $ isPartialFile (eventPath e)
                                       , all (eventPath e /~~) ignores
                                       ]


-- TODO: use a better matching library to do this
(/~~)      :: FilePath -> GlobPattern -> Bool
fp /~~ pat = (encodeString fp) /~ pat



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
      watchTree mgr fp aPred handleEvent'
      stopAct
      stopManager mgr
