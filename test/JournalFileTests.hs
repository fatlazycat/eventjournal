module JournalFileTests where
import           Test.Tasty
import           Test.Tasty.Hspec

specTests :: IO TestTree
specTests = testGroup "journal file tests" <$> sequence [temp2]

temp2 :: IO TestTree
temp2 = testSpec "file creation" $
  describe "Addition" $ do
    it "2 + 2 is 4" $
      (2 + 2) `shouldBe` 4
