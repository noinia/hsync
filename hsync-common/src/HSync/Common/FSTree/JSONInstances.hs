{-# Language ViewPatterns #-}
{-# Language OverloadedStrings #-}
module HSync.Common.FSTree.JSONInstances where


import Control.Applicative((<$>), (<*>))
import Control.Monad(mzero)

import Data.Aeson

import Storage.Hashed.Tree( Tree, TreeItem(..), Blob(..))

import HSync.Common.FSTree

import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict as H
import qualified Storage.Hashed.Tree as HT

-- | Import this module to import the ToJSON and FromJSON Instances on trees.


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
