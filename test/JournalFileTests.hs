module JournalFileTests where
import           Test.Tasty
import           Test.Tasty.Hspec

specTests :: IO TestTree
specTests = testGroup "journal file tests" <$> sequence [fileCreation, fileCreation2]

fileCreation :: IO TestTree
fileCreation = testSpec "file creation" $
  describe "Addition" $ do
    it "1 + 1 is 2" $
      (1 + 1) `shouldBe` 2

fileCreation2 :: IO TestTree
fileCreation2 = testSpec "file creation" $
  describe "Addition" $ do
    it "2 + 2 is 4" $
      (2 + 2) `shouldBe` 4
