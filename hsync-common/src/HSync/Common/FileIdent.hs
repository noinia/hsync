{-# Language TemplateHaskell    #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language DeriveDataTypeable #-}
module HSync.Common.FileIdent( FileIdent(..)
                             , fileIdent

                             , HasFileIdent(..)

                             , getDateTime
                             , isDirectory
                             , isFile
                             , isNonExistent

                             , protectedByFI
                             , checkFileIdent
                             , ErrorDescription

                             , checkMTime
                             ) where

import Control.Applicative((<$>),(<*>))

import Data.Aeson.TH
import Data.Data(Data, Typeable)
import Data.SafeCopy(base, deriveSafeCopy)
import Data.Monoid
import Data.Text(Text)
import Yesod.Core

import HSync.Common.DateTime
import HSync.Common.Types
import HSync.Common.Import

import HSync.Common.AtomicIO


import System.Directory(doesDirectoryExist)

import qualified Data.Text as T

--------------------------------------------------------------------------------

data FileIdent = NonExistent
               | Directory DateTime
               | File      DateTime
               deriving (Show,Read,Eq,Data,Typeable)

$(deriveJSON defaultOptions ''FileIdent)
$(deriveSafeCopy 0 'base ''FileIdent)

getDateTime (Directory t) = t
getDateTime (File t)      = t
getDateTime _             = error "getDateTime: NonExistent."

isDirectory               :: FileIdent -> Bool
isDirectory (Directory _) = True
isDirectory _             = False

isFile          :: FileIdent -> Bool
isFile (File _) = True
isFile _        = False

isNonExistent             :: FileIdent -> Bool
isNonExistent NonExistent = True
isNonExistent _           = False



dirPrefix :: Text
dirPrefix = "directory_"

filePrefix :: Text
filePrefix = "file_"

instance PathPiece FileIdent where
    toPathPiece NonExistent   = "nonexistent"
    toPathPiece (Directory d) = dirPrefix  <> toPathPiece d
    toPathPiece (File      d) = filePrefix <> toPathPiece d
    fromPathPiece t | t == "nonexistent"        = Just NonExistent
                    | t `startsWith` dirPrefix  = Directory <$> f dirPrefix
                    | t `startsWith` filePrefix = File      <$> f filePrefix
                    | otherwise                 = Nothing
        where
          f s = fromPathPiece $ T.drop (T.length s) t


--------------------------------------------------------------------------------
-- | Types that can be converted to file idents

class HasFileIdent c where
  toFileIdent :: c -> FileIdent

instance HasFileIdent FileIdent where
  toFileIdent = id

instance HasFileIdent a => HasFileIdent (Maybe a) where
  toFileIdent = maybe NonExistent toFileIdent

instance (HasFileIdent a, HasFileIdent b) => HasFileIdent (Either a b) where
  toFileIdent = either toFileIdent toFileIdent

--------------------------------------------------------------------------------
-- | Computing and comparing File Idents

-- | Given a path, compute the file ident
fileIdent    :: (Functor m, MonadIO m) => FilePath -> m FileIdent
fileIdent fp = exists fp >>= \t -> case t of
                 (False,False) -> return NonExistent
                 (_,    True)  -> Directory <$> mT fp
                 (True,False)  -> File      <$> mT fp
    where
      mT = liftIO . modificationTime

type ErrorDescription = [Text]


-- Run a handler/computation on the input, if the file idents
-- match. Specifically: protectedByFI fi fp hName h runs the computation h, if
-- the file ident of fp is fi. If this is not the case, an error message is
-- generated that includes the name hName of the action/computation that we
-- wanted to run.
protectedByFI               :: (Functor m, MonadIO m)
                            => FileIdent -> FilePath -> Text -> m a
                            -> m (Either ErrorDescription a)
protectedByFI fi fp hName h = do
  me <- checkFileIdent fi fp
  case me of
    Nothing -> h >>= return . Right
    Just e  -> return . Left . insertHName hName $ e

    where
      insertHName   :: Text -> [Text] -> [Text]
      insertHName n = map ((n <> ": ") <>)


-- | Check the fileId. If the result is 'Nothing' then there were no errors found
-- otherwise, we give a description of the error

checkFileIdent       :: (Functor m, MonadIO m)
                     => FileIdent -> FilePath -> m (Maybe ErrorDescription)
checkFileIdent exp p = do
  (f,d) <- exists p
  t     <- if f then modificationTime p else return undefined
  return $ checkFileIdent' f d t exp


checkFileIdent' :: Bool       -- ^ isFile           fp
                -> Bool       -- ^ isDirectory      fp
                -> DateTime   -- ^ modificationTime fp
                -> FileIdent  -- ^ Expected file ident
                -> Maybe ErrorDescription
checkFileIdent' False False _ NonExistent    = Nothing
checkFileIdent' True  False _ NonExistent    = Just ["File found, no file or directory expected."]
checkFileIdent' True  _     _ NonExistent    = Just ["Directory found, no file or directory expected."]
checkFileIdent' False False _ (Directory _)  = Just ["Nothing found, directory expected."]
checkFileIdent' True  False _ (Directory _)  = Just ["File found, directory expected."]
checkFileIdent' _     True  t (Directory t') = checkMTime' t t'
checkFileIdent' False False _ (File _)       = Just ["Nothing found, file expected."]
checkFileIdent' True  False t (File t')      = checkMTime' t t'
checkFileIdent' True  _     _ (File _)       = Just ["Directory found, file expected."]


-- | Check the modification time of a file.
checkMTime     :: (Functor m, MonadIO m)
               => FilePath -> DateTime -> m (Maybe ErrorDescription)
checkMTime p d = checkMTime' d <$> liftIO (modificationTime p)

checkMTime' :: DateTime -> DateTime -> Maybe ErrorDescription
checkMTime' a b
  | a == b    = Nothing
  | otherwise = Just ["modification date mismatch."]




-- data FileIdentComparison = Same FileIdent
--                          | ModificationTimeMismatch FileIdent
--                          | WrongType { found :: FileIdent
--                                      , expected :: FileIdent
--                                      }
--                            deriving (Show,Read,Eq,Data,Typeable)


--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- | Helper functions
startsWith :: Text -> Text -> Bool
startsWith = flip T.isPrefixOf


noError :: Monad m => m (Maybe a)
noError = return Nothing

err   :: Monad m => Text -> m (Maybe ErrorDescription)
err t = return . Just $ [t]
