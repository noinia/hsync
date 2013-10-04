module HSync.Common.AtomicIO where

import Control.Exception.Base(catchJust)


import Control.Applicative(pure, (<$>), (<*>))
import Control.Monad.IO.Class(liftIO, MonadIO(..))

import System.Directory( doesDirectoryExist)

import System.IO(writeFile)
import System.IO.Error(isDoesNotExistErrorType, ioeGetErrorType)
import System.Posix.Files(fileSize, getFileStatus)

import GHC.IO.Exception(IOErrorType(..))

import System.Lock.FLock


atomicallyWriteIO fp act = catchJust doesNotExistException
                           (atomicallyIO fp act)
                           (\_ -> writeFile fp "" >> atomicallyWriteIO fp act)
    where


-- | Get an lock on the file so we can work on it. If the fp points to a directory
-- this will be a shared lock, otherwise a Exclusive lock
atomicallyIO        :: FilePath -> IO a -> IO a
atomicallyIO fp act = catchJust inAppropriateTypeException
                      (withLock fp Exclusive Block act)
                      (\_ -> withLock fp Shared Block act)
    where
      inAppropriateTypeException e
          | ioeGetErrorType e == InappropriateType = Just ()
          | otherwise                              = Nothing


doesNotExistException  e = if isDoesNotExistErrorType (ioeGetErrorType e)
                           then Just () else Nothing


-- | Checks if the file exists and has filezize > 0B. We cannot just use
-- doesFileExist since the flock (atomicIO) requires the file to exist. Hence,
-- the atomicIO actions create a 0B size file if the file did not exist before.
isPropperFile    :: FilePath -> IO Bool
isPropperFile fp = catchJust doesNotExistException
                   ((\s -> fileSize s > fromInteger 0) <$> getFileStatus fp)
                   (const $ return False)


-- | Check if a file is a regular file or a directory
exists    :: MonadIO m => FilePath -> m (Bool, Bool)
exists fp = liftIO $ (\a b -> (a,b)) <$> isPropperFile      fp
                                     <*> doesDirectoryExist fp
