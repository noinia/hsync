{-# LANGUAGE TypeSynonymInstances #-}
{-# Language MultiParamTypeClasses #-}
module FSTreeTests where

import Control.Applicative


import Data.Monoid

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import HSync.Common.Types
import HSync.Common.FSTree

import Test.QuickCheck.Instances

import qualified Data.Foldable as F

import Data.Text

-- instance Arbitrary FileName where
--   arbitrary = (arbitrary :: Gen Text)




instance (Arbitrary a, Measured m a) => Arbitrary (File m a) where
  arbitrary = file <$> arbitrary
                   <*> arbitrary

-- instance (Arbitrary a, Measured m a) => Arbitrary (Directory m a) where
--   arbitrary = (\n d sds fs -> Directory n d (measure d) sds fs)
--               <$> arbitrary
--               <*> arbitrary
--               <*> arbitrary
--               <*> arbitrary
--   shrink d@(Directory n x _ sds fs) = [ emptyDirectory n x
--                                       , d { files = mempty , subDirectories = mempty}
--                                       , d { files = mempty }
--                                       , d { subDirectories = mempty}
--                                       ] ++ F.toList sds ++
--                                       [ Directory n x (measure x) sds' fs'
--                                       | (n,x,sds',fs') <- shrink (n,x,sds,fs)
--                                       ]

-- fsTreeTests :: TestTree
-- fsTreeTests = testGroup "FSTree"
--   [ andLastTests
--   , QC.testProperty "Find x . AddFile x = True" $
--     \n x dir -> fileExists [n] $ addFile (file n (x :: Int)) dir
--   ]


-- andLastTests = testGroup "andLast"
--   [ QC.testProperty "snd . andLast = head . reverse" $
--     \x xs -> let ys = x : xs :: [Int] in
--              snd (andLast ys) == (head $ reverse ys)
--   , QC.testProperty "fst . andLast = init " $
--     \x xs -> let ys = x : xs :: [Int] in
--              fst (andLast ys) == init ys
--   ]
