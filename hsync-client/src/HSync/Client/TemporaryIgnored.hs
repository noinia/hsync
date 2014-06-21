{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module HSync.Client.TemporaryIgnored( initializeTemporaryIgnored
                                    , isTemporarilyIgnored
                                    , temporarilyIgnore
                                    , unIgnoreIn
                                    , unIgnore

                                    , whileIgnored
                                    , whileIgnoredFor

                                    , MicroSeconds(..)
                                    ) where


import Prelude hiding (FilePath)


import Control.Applicative
import Control.Concurrent(forkIO, threadDelay)
import Control.Concurrent.STM(atomically)
import Control.Concurrent.STM.TVar

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Class

import Data.Monoid
import Data.Set(Set)


import Filesystem.Path.CurrentOS

import HSync.Client.ActionT(Action, getActionState, ActionT(..), cloneInIO)
import HSync.Client.Logger
import HSync.Client.Sync( TemporaryIgnoreFiles
                        , FilePathIgnoringTrailingSlash(..)
                        )

import qualified Data.Set  as S

--------------------------------------------------------------------------------

-- | Check if a file is on the temporarily ignored list
isTemporarilyIgnored    :: FilePath -> Action Bool
isTemporarilyIgnored fp = getActionState >>= isIgnored
  where
    -- isIgnored v = S.member fp <$> readTVarIO v
    isIgnored v = do s <- liftIO $ readTVarIO v
                     debugM "TemporaryIgnored.isTemporarilyIgnored" $ mconcat
                       [ "Temporarily ignored files: "
                       , show s
                       ]
                     return $ S.member (IgnoreSlash fp) s

-- | Temporarily ignore a file, while running act
whileIgnored        :: FilePath -> (FilePath -> IO ()) -> Action ()
whileIgnored fp ioA = getActionState >>= \v -> liftIO $
                        temporarilyIgnore v fp ioA unIgnore

whileIgnoredFor              :: MicroSeconds -> FilePath
                             -> (FilePath -> IO ())
                             -> Action ()
whileIgnoredFor delay fp ioA = getActionState >>= \v -> liftIO $
                                 temporarilyIgnore v fp ioA (unIgnoreIn delay)


-- whileIgnored' fp act unIg = getActionState >>= whileIgnored'' fp act unIg



-- whileIgnored'' fp act unIg ignores = act'
--   where
--     act' = runResourceT $ do
--       _ <- allocate (temporarilyIgnore ignores fp)
--                     (unIg ignores)
--       lift $ act




--------------------------------------------------------------------------------
-- |

temporarilyIgnore                 :: IgnoredFiles
                                  -> FilePath  -- ^ The file to ignore
                                  -> (FilePath -> IO a) -- ^ Computation to run while
                                                        -- file is ignored
                                  -> (IgnoredFiles -> FilePath -> IO ())
                                                        -- ^ unignore action
                                  -> IO a
temporarilyIgnore igs fp act unIg = runResourceT $ do
      (_,fp') <- allocate (addTemporarilyIgnore igs fp)
                          (unIg igs)
      lift $ act fp'


--------------------------------------------------------------------------------

newtype MicroSeconds = MicroSeconds Int
                       deriving (Show,Read,Eq,Ord,Num,Real,Enum,Integral)

--------------------------------------------------------------------------------
-- | Primitives operation on TemporaryIgnoreFiles


type IgnoredFiles = TVar TemporaryIgnoreFiles

initializeTemporaryIgnored :: IO IgnoredFiles
initializeTemporaryIgnored = newTVarIO mempty

-- | Add to the temporary ignore list
addTemporarilyIgnore        :: IgnoredFiles -> FilePath -> IO FilePath
addTemporarilyIgnore igs fp = atomically (add igs) >> return fp
  where
    add = flip modifyTVar (S.insert $ IgnoreSlash fp)

-- | Unignore the file after waiting `delay` microseconds
--  This function starts a new thread to wait, so it immediately
-- returns.
unIgnoreIn                           :: MicroSeconds
                                     -> IgnoredFiles
                                     -> FilePath -> IO ()
unIgnoreIn (MicroSeconds delay) v fp = forkIO (threadDelay delay >> unIgnore v fp)
                                       >>
                                       return ()

unIgnore        :: IgnoredFiles -> FilePath -> IO ()
unIgnore igs fp = do
                    atomically . delete $ igs
                    print $ "unignored " <> show fp
  where
    delete = flip modifyTVar (S.delete $ IgnoreSlash fp)
