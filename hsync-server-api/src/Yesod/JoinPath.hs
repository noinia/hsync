{-# Language OverloadedStrings #-}
module Yesod.JoinPath where


import Blaze.ByteString.Builder(Builder)
import Blaze.ByteString.Builder.Char.Utf8 (fromText)

import Control.Arrow((***))

import Data.Monoid
import Data.Text(Text)


import Network.HTTP.Types                 (encodePath)

import Yesod.Core hiding(joinPath)

import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as TE

class HasJoinPath site where
  -- copied from the Yesod class
  joinPath :: site -> Text -> [Text] -> [(Text,Text)] -> Builder
  joinPath _ ar pieces' qs' =
    fromText ar <> encodePath pieces qs
      where
        pieces = if null pieces' then [""] else map addDash pieces'
        qs = map (TE.encodeUtf8 *** go) qs'
        go "" = Nothing
        go x = Just $ TE.encodeUtf8 x
        addDash t
            | T.all (== '-') t = T.cons '-' t
            | otherwise = t
