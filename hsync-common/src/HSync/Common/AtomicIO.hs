{-# LANGUAGE FlexibleContexts #-}
module HSync.Common.AtomicIO where


import Control.Exception.Lifted(catchJust)

import Control.Monad.Trans.Control

import Control.Applicative(pure, (<$>), (<*>))
import Control.Monad.IO.Class(liftIO, MonadIO(..))

import System.Directory( doesDirectoryExist, removeFile )

import System.IO(writeFile)
import System.IO.Error(isDoesNotExistErrorType, ioeGetErrorType)
import System.Posix.Files(fileSize, getFileStatus)

import GHC.IO.Exception(IOErrorType(..))

import System.Lock.FLock


-- | Gets an exclusive write lock on fp, and runs act. If the file d oes not exist
-- this action creates an empty file, and then runs the action.
atomicallyWriteIO        :: (MonadIO m, MonadBaseControl IO m) => FilePath -> m a -> m a
atomicallyWriteIO fp act = catchJust doesNotExistException
                           (atomicallyIO fp act)
                           (\_ ->    createFile fp
                                  >> atomicallyWriteIO fp (deleteFile fp >> act)
                           )


createFile :: MonadIO m => FilePath -> m ()
createFile = liftIO . flip writeFile ""

deleteFile :: MonadIO m => FilePath -> m ()
deleteFile = liftIO . removeFile




doesNotExistException    :: IOError -> Maybe ()
doesNotExistException  e = if isDoesNotExistErrorType (ioeGetErrorType e)
                           then Just () else Nothing


-- | Get an lock on the file so we can work on it. If the fp points to a directory
-- this will be a shared lock, otherwise a Exclusive lock
-- atomicallyIO          :: MonadIO m => FilePath -> IO a -> m a
-- atomicallyIO fp act = liftIO $ catchJust inAppropriateTypeException
--                       (withLock fp Exclusive Block act)
--                       (\_ -> withLock fp Shared Block act)
--     where
--       inAppropriateTypeException e
--           | ioeGetErrorType e == InappropriateType = Just ()
--           | otherwise                              = Nothing


-- | Get an (exclusive) lock on the file, and run act. If the file path pointed
-- to does not exist, this throws an exception.
atomicallyIO    :: (MonadIO m, MonadBaseControl IO m) => FilePath -> m a -> m a
atomicallyIO fp = withLock fp Exclusive Block







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
