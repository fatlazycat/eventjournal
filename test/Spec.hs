import           EventTests
import           JournalFileTests
import           Test.Tasty
import           Test.Tasty.Ingredients.Rerun
import           Test.Tasty.Runners

main :: IO ()
main = allTests >>= defaultMainWithIngredients [rerunningTests [consoleTestReporter]]

allTests :: IO TestTree
allTests = testGroup "All tests" <$>
  sequence [JournalFileTests.specTests
           , EventTests.specTests]
