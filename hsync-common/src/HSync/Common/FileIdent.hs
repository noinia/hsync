{-# Language  OverloadedStrings
  #-}
module HSync.Common.FileIdent( FileIdent(..)
                             , fileIdent
                             , checkFileIdent
                             , ErrorDescription
                             , fileHash
                             , fileDate
                             ) where



import Control.Applicative((<$>),(<*>))
import Data.Monoid
import Data.Text(Text)
import Yesod.Core

import HSync.Common.Types
import HSync.Common.Import

import HSync.Common.AtomicIO(isPropperFile)


import System.Directory( doesDirectoryExist
                       , getModificationTime
                       )

import qualified Data.Text as T

--------------------------------------------------------------------------------

type HashedFile = Text

data FileIdent = NonExistent
               | Directory
               | FileDate DateTime
               | FileHash HashedFile
               deriving (Show,Read,Eq)

dtPrefix,hashPrefix :: Text
dtPrefix   = "dt_"
hashPrefix = "hash_"

instance PathPiece FileIdent where
    toPathPiece NonExistent   = "nonexistent"
    toPathPiece Directory     = "directory"
    toPathPiece (FileDate dt) = dtPrefix   <> toPathPiece dt
    toPathPiece (FileHash h)  = hashPrefix <> showT h
    fromPathPiece t | t == "nonexistent"        = Just NonExistent
                    | t == "directory"          = Just Directory
                    | t `startsWith` dtPrefix   = FileDate <$> f dtPrefix
                    | t `startsWith` hashPrefix = FileHash <$> f hashPrefix
                    | otherwise                 = Nothing
        where
          f s = fromPathPiece $ T.drop (T.length s) t

-- | Given a path, compute a fileIdent using the file hash
fileHash    :: MonadIO m => FilePath -> m FileIdent
fileHash _  = return $ FileHash "dummy" --TODO


-- | Given a path, compute a fileIdent using the file modification time
fileDate    :: MonadIO m => FilePath -> m FileIdent
fileDate fp = liftIO $ FileDate <$> modificationTime fp


--------------------------------------------------------------------------------
-- | Computing and comparing File Idents

-- | Given a path, compute the file ident
fileIdent    :: MonadIO m => FilePath -> m FileIdent
fileIdent fp = exists fp >>= \t -> case t of
                 (False,False) -> return NonExistent
                 (False,True)  -> return Directory
                 (True,False)  -> fileDate fp
                 (True,True)   -> error "fp is both a file and a directory!?"


type ErrorDescription = [Text]

-- | Check the fileId. If the result is 'Nothing' then there were no errors found
-- otherwise, we give a description of the error
checkFileIdent                :: MonadIO m => FileIdent -> FilePath ->
                                   m (Maybe ErrorDescription)
checkFileIdent NonExistent  p = exists p >>= \t -> case t of
                                  (False,False) -> noError
                                  (True,False)  ->
                                      fiErr "no file expected but file found."
                                  (False,True)  ->
                                      fiErr "no file expected but directory found."
                                  (True,True)   ->
                                      fiErr "fp is both a file and a directory!?"
checkFileIdent Directory    p = protect (liftIO $ doesDirectoryExist p)
                                        noError
                                        (fiErr "directory expected but not found.")
checkFileIdent (FileDate d) p = protect (modificationTime p >>= return . (== d))
                                        noError
                                        (fiErr "file modification time does not match.")
checkFileIdent (FileHash _) _ = fiErr "checking on file hash not implemented yet."

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- | Helper functions

startsWith :: Text -> Text -> Bool
startsWith = flip T.isPrefixOf


noError :: Monad m => m (Maybe a)
noError = return Nothing

fiErr   :: Monad m => Text -> m (Maybe ErrorDescription)
fiErr t = return . Just $ [t]

-- | Get the file modification time
modificationTime    :: MonadIO m => FilePath -> m DateTime
modificationTime fp = liftIO $ DateTime <$> getModificationTime fp



-- | Check if a file is a regular file or a directory
exists    :: MonadIO m => FilePath -> m (Bool, Bool)
exists fp = liftIO $ (\a b -> (a,b)) <$> isPropperFile      fp
                                     <*> doesDirectoryExist fp
