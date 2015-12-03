module JournalFileTests where

import           Test.Tasty
import           Test.Tasty.Hspec
import System.Posix.Memory

specTests :: IO TestTree
specTests = testGroup "journal file tests" <$> sequence [temp2]

temp2 :: IO TestTree
temp2 = testSpec "OSX Specific - needs changing" $
  describe "Page size" $ do
    it "we expect page size to be 4096" $
      sysconfPageSize `shouldBe` 4096
