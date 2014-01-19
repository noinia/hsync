module Test.HSync.Common.FSTree where

import Data.Monoid

import Test.Tasty
import Test.Tasty.QuickCheck as QC



import HSync.Common.FSTree.Basic

import qualified Data.Text as T

basicFSTree = testGroup "FSTree.Basic"
              [ QC.testProperty "sillyName" $
                \n l -> (File n l :: File Int) `hasFileName` n
              , QC.testProperty "False name " $
                \n l -> (File n l :: File Int) `hasFileName` "foo"
              ]


instance Arbitrary T.Text where
  arbitrary = fmap T.pack arbitrary
