{-# Language TemplateHaskell #-}
{-# Language ViewPatterns #-}
{-# Language OverloadedStrings #-}
module HSync.Common.FSTree where

import Control.Applicative((<$>), (<*>))
import Control.Monad(mzero)

import Storage.Hashed.Plain(readPlainTree)
import Storage.Hashed.Darcs(darcsTreeHash)
import Storage.Hashed.Hash
import Storage.Hashed.AnchoredPath
import Storage.Hashed.Tree( Tree, TreeItem(..), Blob(..))


import System.Directory

import Storage.Hashed.Index

import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict as H
import qualified Storage.Hashed.Tree as HT
import qualified Storage.Hashed.Index as I

import Data.Aeson
import Data.Aeson.TH


$(deriveJSON id ''Name)
$(deriveJSON id ''AnchoredPath)
$(deriveJSON id ''Hash)


instance ToJSON (Blob m) where
    toJSON (Blob _ h) = object ["Blob" .= h]

instance Monad m => FromJSON (Blob m) where
    parseJSON (Object (H.toList -> [(key, value)]))
              | key == "Blob" = Blob (return $ B.pack []) <$> parseJSON value
    parseJSON _               = mzero


instance ToJSON (TreeItem m) where
    toJSON (File b)    = object ["File"    .= b]
    toJSON (SubTree t) = object ["SubTree" .= t]
    toJSON (Stub _ h)  = object ["Stub"    .= h]

instance Monad m => FromJSON (TreeItem m) where
    parseJSON (Object (H.toList -> [(key, value)]))
              | key == "File"    = File                       <$> parseJSON value
              | key == "SubTree" = SubTree                    <$> parseJSON value
              | key == "Stub"    = Stub (return HT.emptyTree) <$> parseJSON value
              | otherwise        = mzero

instance ToJSON (Tree m) where
    toJSON t = object [ "Hash"     .= HT.treeHash t
                      , "Elements" .= HT.listImmediate t
                      ]

instance Monad m => FromJSON (Tree m) where
    parseJSON (Object (H.toList -> [ (key1, value1)
                                   , (key2, value2)
                                   ]))
              | key1 == "Hash"  &&
                key2 == "Elements" = HT.makeTreeWithHash
                                     <$> parseJSON value2
                                     <*> parseJSON value1
              | otherwise          = mzero
    parseJSON _                    = mzero


printTree = print . map (\(a,b) -> (a, HT.itemHash b)) . HT.list


test = do
  -- print =<< getCurrentDirectory
  tree <- readPlainTree "/Users/frank/tmp/synced"
  print "plain tree"
  printTree tree
  idx <- updateIndexFrom "/Users/frank/tmp/index" darcsTreeHash tree
  -- idx  <- readIndex "/Users/frank/tmp/index" darcsTreeHash
  idxTree <- updateIndex idx
  print "unexpanded tree?"
  printTree idxTree
  fTree <- updateIndex $ I.filter (\a b -> True) idx
  print "FTree"
  printTree fTree
  tree' <- HT.expand idxTree
  print "expanded indexed tree"
  printTree tree'
