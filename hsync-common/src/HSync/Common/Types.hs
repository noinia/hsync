{-# LANGUAGE TemplateHaskell    #-}
{-# Language  OverloadedStrings #-}
module HSync.Common.Types( UserIdent
                         , Password
                         , HashedPassword
                         , ClientIdent
                         -- Paths
                         , Path(..)
                         , toFilePath
                         , isSubPathOf

                         , SubPath
                         , FileName

                         ) where



import Prelude

import Data.Aeson.TH

import Data.Text(Text)
import Yesod.Core

import Data.Function(on)
import Data.List(intercalate, isPrefixOf)

import Text.Read(readMaybe)


import qualified Data.Text as T

--readT = read . T.unpack

--------------------------------------------------------------------------------

type UserIdent = Text
type Password = Text
type HashedPassword = Text

type ClientIdent = Text

--------------------------------------------------------------------------------

type FileName = Text

type SubPath = [FileName]


data Path = Path { owner   :: UserIdent
                 , subPath :: SubPath
                 }
            deriving (Show,Read,Eq,Ord)

$(deriveJSON defaultOptions ''Path)

instance PathMultiPiece Path where
    toPathMultiPiece (Path u ps) = u : ps -- map T.pack ps
    fromPathMultiPiece (u:ps) = Just $ Path u ps -- . map T.unpack
    fromPathMultiPiece _      = Nothing

toFilePath                     :: Text -> Path -> FilePath
toFilePath baseDir (Path u ps) = intercalate "/" . map T.unpack $ baseDir:u:ps

isSubPathOf       :: Path -> Path -> Bool
p `isSubPathOf` q = let fp = toFilePath "" p
                        fq = toFilePath "" q in
                    fp `isPrefixOf` fq
