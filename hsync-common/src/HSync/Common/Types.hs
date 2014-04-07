{-# Language TemplateHaskell    #-}
{-# Language OverloadedStrings #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language DeriveDataTypeable #-}
module HSync.Common.Types( UserIdent(..)
                         , Password(..)
                         , HashedPassword(..)
                         , ClientIdent(..)
                         -- Paths
                         , Path(..)
                         , toFilePath
                         , isSubPathOf

                         , SubPath
                         , FileName

                         ) where



import Prelude

import Data.Aeson.TH
import Data.Data(Data, Typeable)
import Data.Function(on)
import Data.List(intercalate, isPrefixOf)
import Data.SafeCopy(SafeCopy(..), base, deriveSafeCopy)
import Data.Text(Text)

import Text.Read(readMaybe)

import Yesod.Core


import qualified Data.Text as T

--------------------------------------------------------------------------------

newtype UserIdent = UserIdent { unUI :: Text }
                    deriving (Show,Read,Eq,Ord,Data,Typeable,
                              SafeCopy,PathPiece,FromJSON,ToJSON)

newtype Password = Password { unPassword :: Text }
                    deriving (Show,Read,Eq,Ord,Data,Typeable,
                              SafeCopy,PathPiece,FromJSON,ToJSON)

newtype HashedPassword = HashedPassword { unHashedPassword :: Text }
                    deriving (Show,Read,Eq,Ord,Data,Typeable,
                              SafeCopy,PathPiece,FromJSON,ToJSON)

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
    toPathMultiPiece (Path u ps) = toPathPiece u : ps -- map T.pack ps
    fromPathMultiPiece (u:ps) = Just $ Path (UserIdent u) ps -- . map T.unpack
    fromPathMultiPiece _      = Nothing

toFilePath                                 :: Text -> Path -> FilePath
toFilePath baseDir (Path (UserIdent u) ps) = intercalate "/" . map T.unpack $ baseDir:u:ps

isSubPathOf       :: Path -> Path -> Bool
p `isSubPathOf` q = let fp = toFilePath "" p
                        fq = toFilePath "" q in
                    fp `isPrefixOf` fq
