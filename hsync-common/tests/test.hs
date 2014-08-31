{-# Language GeneralizedNewtypeDeriving #-}
{-# Language StandaloneDeriving #-}
{-# Language ScopedTypeVariables #-}

import Control.Applicative


import Test.Tasty
-- import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.Typeable
import Data.Aeson(toJSON, fromJSON)
import Data.List
import Data.Ord
import Data.Text(Text,pack)

import FSTreeTests

import HSync.Common.DateTime
import HSync.Common.Types

import Yesod.Core


import qualified Data.Aeson as Aeson

--------------------------------------------------------------------------------

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties] --, unitTests]

properties :: TestTree
properties = testGroup "Properties" [ dateTimeProperties
                                    , pathPieceTests
                                    , jsonTests
--                                    , fsTreeTests
                                    ]


--------------------------------------------------------------------------------


pathPieceTests :: TestTree
pathPieceTests = testGroup "PathPiece (Identity) Tests"
  [ pathPieceIdTest "DateTime"       (Proxy :: Proxy DateTime)
  , pathPieceIdTest "UserIdent"      (Proxy :: Proxy UserIdent)
  , pathPieceIdTest "Password"       (Proxy :: Proxy Password)
  , pathPieceIdTest "HashedPassword" (Proxy :: Proxy HashedPassword)
  , pathPieceIdTest "ClientIdent"    (Proxy :: Proxy ClientIdent)

  , pathMultiPieceIdTest "Path"      (Proxy :: Proxy Path)
  ]


pathPieceIdTest   :: forall a. (Eq a, Arbitrary a, PathPiece a, Show a)
                  => String -> Proxy a -> TestTree
pathPieceIdTest s _ = QC.testProperty ("fromPathPiece . toPathPiece = id : " ++ s) $
    \x -> Just x == (fromPathPiece (toPathPiece x) :: Maybe a)


pathMultiPieceIdTest     :: forall a. (Eq a, Arbitrary a, PathMultiPiece a, Show a)
                         => String -> Proxy a -> TestTree
pathMultiPieceIdTest s _ = QC.testProperty descr $
    \x -> Just x == (fromPathMultiPiece (toPathMultiPiece x) :: Maybe a)
  where
    descr = "fromPathMultiPiece . toPathMultiPiece = id : " ++ s


----------------------------------------

jsonTests :: TestTree
jsonTests = testGroup "JSON Identity Tests"
  [ fromToJSONIdTest "DateTime"       (Proxy :: Proxy DateTime)
  , fromToJSONIdTest "UserIdent"      (Proxy :: Proxy UserIdent)
  , fromToJSONIdTest "Password"       (Proxy :: Proxy Password)
  , fromToJSONIdTest "HashedPassword" (Proxy :: Proxy HashedPassword)
  , fromToJSONIdTest "ClientIdent"    (Proxy :: Proxy ClientIdent)
  , fromToJSONIdTest "Path"           (Proxy :: Proxy Path)
  ]

fromToJSONIdTest     :: forall a. (Eq a, Arbitrary a, FromJSON a, ToJSON a, Show a)
                     => String -> Proxy a -> TestTree
fromToJSONIdTest s _ = QC.testProperty ("fromJSON . toJSON = id: " ++ s) $
    \x -> Aeson.Success x == (fromJSON (toJSON x) :: Aeson.Result a)


----------------------------------------


dateTimeProperties :: TestTree
dateTimeProperties = testGroup "DateTime"
  [ QC.testProperty "read . show = id" $
      \dt -> dt == (read (show dt) :: DateTime)
  ]



----------------------------------------

unitTests = testGroup "Unit tests"
  [ testCase "List comparison (different length)" $
      [1, 2, 3] `compare` [1,2] @?= GT

  -- the following test does not hold
  , testCase "List comparison (same length)" $
      [1, 2, 3] `compare` [1,2,2] @?= LT
  ]

--------------------------------------------------------------------------------
-- | Arbitrary Instances

deriving instance Arbitrary DateTime


newtype AlphaNumericText = AlphaNum { unAN :: Text }
                           deriving (Show,Read,Eq,Ord)


instance Arbitrary AlphaNumericText where
  arbitrary = AlphaNum . pack <$> genSafeList
    where
      safeChars = ['a'..'z'] ++ ['0'..'9']

      genSafeChar = elements safeChars
      genSafeList = listOf genSafeChar

instance Arbitrary UserIdent where
  arbitrary = toUI <$> arbitrary
    where
      toUI (AlphaNum t) = case userIdent t of
        Left _  -> error "Arbitrary UserIdent: Trying to generate an invalid UserIdent."
        Right u -> u



deriving instance Arbitrary Password
deriving instance Arbitrary HashedPassword
deriving instance Arbitrary ClientIdent

instance Arbitrary Path where
  arbitrary = Path <$> arbitrary
                   <*> arbitrary
