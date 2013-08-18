module HSync.Common.AtomicIO where

import Control.Exception.Base(catchJust)


import Control.Applicative(pure, (<$>), (<*>))
import System.IO(writeFile)
import System.IO.Error(isDoesNotExistErrorType, ioeGetErrorType)
import System.Posix.Files(fileSize, getFileStatus)


import System.Lock.FLock


atomicallyWriteIO fp act = catchJust doesNotExistException
                           (atomicallyIO fp act)
                           (\_ -> writeFile fp "" >> atomicallyWriteIO fp act)
    where


atomicallyIO    :: FilePath -> IO a -> IO a
atomicallyIO fp = withLock fp Exclusive Block


doesNotExistException  e = if isDoesNotExistErrorType (ioeGetErrorType e)
                           then Just () else Nothing


-- | Checks if the file exists and has filezize > 0B. We cannot just use
-- doesFileExist since the flock (atomicIO) requires the file to exist. Hence,
-- the atomicIO actions create a 0B size file if the file did not exist before.
isPropperFile    :: FilePath -> IO Bool
isPropperFile fp = catchJust doesNotExistException
                   ((\s -> fileSize s > fromInteger 0) <$> getFileStatus fp)
                   (const $ return False)
