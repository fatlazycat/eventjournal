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
    let ps = 4096
    it "Start 0 and 100 bytes = 0 and page size" $
      syncPoints 0 100 ps `shouldBe` (0,ps)
    it "Start 5000 and 100 bytes = page size and page size" $
      syncPoints 5000 100 ps `shouldBe` (ps,ps)
    it "Start 5000 and 5000 bytes = page size and page size" $
      syncPoints 5000 5000 ps `shouldBe` (ps,ps*2)
    it "Start 10002 and 30501 bytes = 4096 and 32768" $
      syncPoints 10002 30501 ps `shouldBe` (ps*2,ps*8)

temp2 :: IO TestTree
temp2 = testSpec "OSX Specific - needs changing" $
  describe "Page size" $ do
    it "we expect page size to be 4096" $
      sysconfPageSize `shouldBe` 4096
