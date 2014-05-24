module HSync.Client.TemporaryIgnored( initializeTemporaryIgnored
                                    , isTemporarilyIgnored
                                    , temporarilyIgnore
                                    , unIgnoreIn
                                    , unIgnore
                                    , withTemporarilyIgnored
                                    , whileIgnored
                                    ) where


import Prelude hiding (FilePath)

import Control.Applicative
import Control.Concurrent(forkIO, threadDelay)
import Control.Concurrent.STM(atomically)
import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class

import Data.Monoid
import Data.Set(Set)


import Filesystem.Path.CurrentOS

import HSync.Client.ActionT(Action, getActionState)
import HSync.Client.Logger
import HSync.Client.Sync(TemporaryIgnoreFiles)


import qualified Data.Set  as S


initializeTemporaryIgnored :: IO (TVar TemporaryIgnoreFiles)
initializeTemporaryIgnored = newTVarIO mempty




-- | Check if a file is on the temporarily ignored list
isTemporarilyIgnored    :: FilePath -> Action Bool
isTemporarilyIgnored fp = getActionState >>= liftIO . isIgnored
  where
    -- isIgnored v = S.member fp <$> readTVarIO v
    isIgnored v = do s <- readTVarIO v
                     print s
                     return $ S.member fp s



-- | Temporarily ignore a file, while running act
whileIgnored      :: FilePath -> Action () -> Action ()
whileIgnored fp a = withTemporarilyIgnored' fp a unIgnore



-- | Temporarily ignore a file and run an action in the mean time
withTemporarilyIgnored        :: FilePath  -- ^ the path to the file in question
                              -> Int       -- ^ the time (in microseconds) to
                                           -- wait until unignoring the file
                              -> Action () -- ^ the action that to run in the meantime
                              -> Action ()
withTemporarilyIgnored fp t a = withTemporarilyIgnored' fp a (unIgnoreIn t)





-- | Temporarily ignore a file and run a in the meantime. The second Action () is the
-- action that unlocks the file
withTemporarilyIgnored'           :: FilePath
                                  -> Action ()  -- ^ Action to run
                                  -> (FilePath -> Action ())
                                               -- ^ The action that unignores the file
                                  -> Action ()
withTemporarilyIgnored' fp a unIg = temporarilyIgnore fp >> a >> unIg fp

--------------------------------------------------------------------------------

-- | Add to the temporary ignore list
temporarilyIgnore    :: FilePath -> Action ()
temporarilyIgnore fp = debugM "TemporaryIgnored" ("Ignoring " <> show fp)
                       >>
                       getActionState >>= liftIO . atomically . add
  where
    add = flip modifyTVar (S.insert fp)


-- | Unignore the file after waiting `delay` microseconds
--  This function starts a new thread to wait, so it immediately
-- returns.
unIgnoreIn          :: Int -> FilePath -> Action ()
unIgnoreIn delay fp = debugM "TemporaryIgnored" msg
                      >>
                      getActionState >>= \v -> liftIO . const (return ()) . forkIO $
                                               threadDelay delay
                                               >>
                                               unIgnore' fp v
  where
    msg = mconcat [ "Unignoring "
                  , show fp
                  , " in "
                  , show delay
                  , " microseconds."
                  ]


unIgnore    :: FilePath -> Action ()
unIgnore fp = getActionState >>= unIgnore' fp



unIgnore'    :: MonadIO m => FilePath -> TVar TemporaryIgnoreFiles -> m ()
unIgnore' fp = liftIO . atomically . delete
  where
    delete = flip modifyTVar (S.delete fp)
