module Test.HSync.FSTree

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import HSync.Common.FSTree.Basic

basicFSTree = testGroup "FSTree.Basic"
              [ QC.testProperty "sillyName" $
                \n l -> (File n l) `hasFileName` n
              ]
