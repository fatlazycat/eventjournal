module JournalFileTests where

import           System.Posix.Memory
import JournalFile
import           Test.Tasty
import           Test.Tasty.Hspec

specTests :: IO TestTree
specTests = testGroup "journal file tests" <$>
            sequence [pageSize, temp2]

pageSize :: IO TestTree
pageSize = testSpec "Page sign alignment" $
  describe "Page size" $ do
    it "Ptr 0 and 100 bytes = 0 and page size" $
      syncPoints 0 100 sysconfPageSize `shouldBe` (0,sysconfPageSize)

temp2 :: IO TestTree
temp2 = testSpec "OSX Specific - needs changing" $
  describe "Page size" $ do
    it "we expect page size to be 4096" $
      sysconfPageSize `shouldBe` 4096
