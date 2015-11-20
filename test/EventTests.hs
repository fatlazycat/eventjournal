{-# LANGUAGE ScopedTypeVariables #-}

module EventTests where

import           Data.Serialize
import           Event
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Hspec
import           Data.ByteString.Arbitrary

specTests :: IO TestTree
specTests = testGroup "event tests" <$> sequence [serialization]

newtype EventArbitrary = EventArbitrary Event deriving (Show, Eq)

serialization :: IO TestTree
serialization = testSpec "serialization" $
  describe "seriealization" $ do
    it "should encode then decode to the original" $ property $
      \((EventArbitrary x) :: EventArbitrary) -> decode (encode x) == Right x

instance Arbitrary EventArbitrary where
  arbitrary = do
    aId <- arbitrary
    aVer <- arbitrary
    uId <- arbitrary
    t <- arbitrary
    eId <- arbitrary
    p <- arbitrary
    return (EventArbitrary $ Event aId aVer uId t eId (fromABS p))
