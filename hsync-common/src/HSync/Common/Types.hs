{-# Language TemplateHaskell    #-}
{-# Language OverloadedStrings #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language DeriveDataTypeable #-}
module HSync.Common.Types( ErrorMessage
                         , UserIdent
                         , userIdent
                         , unUI

                         , Password(..)
                         , HashedPassword(..)
                         , hashedPassword

                         , ClientIdent(..)
                         -- Paths
                         , Path(..)
                         , toFilePath
                         , isSubPathOf

                         , SubPath
                         , FileName

                         ) where



import Prelude

import Control.Applicative((<$>))

import Data.Aeson.TH
import Data.Char(isAlphaNum)
import Data.Data(Data, Typeable)
import Data.Digest.Pure.SHA(sha1, showDigest)
import Data.Function(on)
import Data.List(intercalate, isPrefixOf)
import Data.SafeCopy(SafeCopy(..), base, deriveSafeCopy)
import Data.Text(Text)

import Text.Read(readMaybe)

import Yesod.Core


import qualified Data.Text                  as T
import qualified Data.ByteString.Lazy.Char8 as B

--------------------------------------------------------------------------------

type ErrorMessage = Text


newtype UserIdent = UserIdent { unUI :: Text }
                    deriving (Show,Read,Eq,Ord,Data,Typeable,
                              SafeCopy,FromJSON,ToJSON)

userIdent   :: Text -> Either ErrorMessage UserIdent
userIdent t
  | T.all userIdentChar t = Right $ UserIdent t
  | otherwise             = Left "Invalid UserIdent. Only alphanumeric characters allowed."

userIdentChar = isAlphaNum

instance PathPiece UserIdent where
  toPathPiece   = unUI
  fromPathPiece = either (const Nothing) Just . userIdent

newtype Password = Password { unPassword :: Text }
                    deriving (Show,Read,Eq,Ord,Data,Typeable,
                              SafeCopy,PathPiece,FromJSON,ToJSON)



newtype HashedPassword = HashedPassword { unHashedPassword :: Text }
                    deriving (Show,Read,Eq,Ord,Data,Typeable,
                              SafeCopy,PathPiece,FromJSON,ToJSON)

hash :: Text -> Text
hash = T.pack . showDigest . sha1 . B.pack . T.unpack

hashedPassword :: Password -> HashedPassword
hashedPassword = HashedPassword . hash . unPassword


newtype ClientIdent = ClientIdent { unCI :: Text }
                    deriving (Show,Read,Eq,Ord,Data,Typeable,
                              SafeCopy,PathPiece,FromJSON,ToJSON)

--------------------------------------------------------------------------------

type FileName = Text

type SubPath = [FileName]


data Path = Path { owner   :: UserIdent
                 , subPath :: SubPath
                 }
            deriving (Show,Read,Eq,Ord,Data,Typeable)

$(deriveJSON defaultOptions ''Path)
$(deriveSafeCopy 0 'base ''Path)

instance PathMultiPiece Path where
    toPathMultiPiece (Path u ps) = toPathPiece u : ps
    fromPathMultiPiece (us:ps) = flip Path ps <$> fromPathPiece us
    fromPathMultiPiece _       = Nothing

toFilePath                                 :: Text -> Path -> FilePath
toFilePath baseDir (Path (UserIdent u) ps) = intercalate "/" . map T.unpack $ baseDir:u:ps

isSubPathOf       :: Path -> Path -> Bool
p `isSubPathOf` q = let fp = toFilePath "" p
                        fq = toFilePath "" q in
                    fp `isPrefixOf` fq
