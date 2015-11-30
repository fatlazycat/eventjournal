{-# LANGUAGE ScopedTypeVariables #-}

module EventTests (
  specTests
                  ) where

import qualified Data.ByteString           as BS
import           Data.ByteString.Arbitrary
import           Data.ByteString.Internal
import           Data.Serialize
import           Event
import           Foreign.ForeignPtr
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import           JournalFile
import           System.Directory
import           System.IO
import           System.IO.MMap
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Hspec

specTests :: IO TestTree
specTests = testGroup "event tests" <$> sequence [serialization]

newtype EventArbitrary = EventArbitrary Event deriving (Show, Eq)

serialization :: IO TestTree
serialization = testSpec "event tests" $
  describe "serialization" $ do
    it "should encode then decode to the original" $ property $
      \((EventArbitrary x) :: EventArbitrary) -> decode (encode x) == Right x
    it "same if we process through a file" $ do
      (fp, h) <- openBinaryTempFile "test-output" "test"
      EventArbitrary x <- generate arbitrary
      let bytes = encode x
      BS.hPut h bytes
      hFlush h
      hClose h
      h' <- openBinaryFile fp ReadMode
      x' <- BS.hGet h' $ BS.length bytes
      removeFile fp
      decode x' `shouldBe` Right x
    it "also if we pass through a memory map" $ do
      EventArbitrary x <- generate arbitrary
      let bytes = encode x
      let (fptr, offset, _) = toForeignPtr bytes
      fp <- openTempFile "test-output" "test" $ toInteger (BS.length bytes)
      (ptr,rawsize,offset',size) <- mmapFilePtr fp ReadWrite Nothing
      withForeignPtr fptr (\x' -> copyBytes (plusPtr ptr offset') (plusPtr x' offset) size)
      bs' <- create size (\x'' -> copyBytes x'' (plusPtr ptr offset') size)
      munmapFilePtr ptr rawsize
      decode bs' `shouldBe` Right x

-- sampleEvent = Event 1 1 1 1 1 $ C.pack "blah"

instance Arbitrary EventArbitrary where
  arbitrary = do
    aId <- arbitrary
    aVer <- arbitrary
    uId <- arbitrary
    t <- arbitrary
    eId <- arbitrary
    p <- arbitrary
    return (EventArbitrary $ Event aId aVer uId t eId (fromABS p))
