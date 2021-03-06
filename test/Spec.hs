import ColorMapperTests
import LuSyntax
import Test.HUnit
import Test.QuickCheck
import LuSyntax
import ColorMapper
import AutoCorrect
import Formatter
import Data.Text


main :: IO ()
main = do
  runTestTT test_distance
  runTestTT test_bestSuggestion
  runTestTT test_bestSuggestionN
  runColorMapperTests
  return ()


test_distance :: Test
test_distance =
  "distance"
    ~: TestList
      [ distance "a" "" ~?= 1,
        distance "" "" ~?= 0,
        distance "" "a" ~?= 1,
        distance "a" "b" ~?= 1,
        distance "abcdef" "abcdef" ~?= 0,
        distance "kitten" "smitten" ~?= 2,
        distance "kitten" "sitting" ~?= 3
      ]

-- | Tests the best suggestion function. This will in turn test the bestSuggestionN function
test_bestSuggestion :: Test
test_bestSuggestion =
    "best suggestion"
        ~: TestList
            [ bestSuggestion "a" [] ~?= Nothing,
              bestSuggestion "a" ["b"] ~?= Just "b",
              bestSuggestion "a" ["a"] ~?= Just "a",
              bestSuggestion "a" ["a", "ab"] ~?= Just "a"
            ]

test_bestSuggestionN :: Test
test_bestSuggestionN = 
  "bestSuggestionN"
    ~: TestList
      [
        bestSuggestionN 4 "a" [] ~?= [],
        bestSuggestionN 23 "a" ["b"] ~?= ["b"],
        bestSuggestionN 1 "a" ["a"] ~?= ["a"],
        bestSuggestionN 2 "a" ["a", "ab"] ~?= ["a", "ab"],
        bestSuggestionN 3 "abd" ["a", "ab", "abd", "aaaaaa", "aaaa"] ~?= ["abd", "ab", "a"]
      ]

-- | parsing and unparsing should not affect the block 
prop_SameMeaning :: Block -> Bool
prop_SameMeaning b = parseFromText (formatBlock b) == Right b


-- this is hanging
-- test that all suggested words are in the dictionary
prop_allWordsInDict :: Int -> String -> [String] -> Bool
prop_allWordsInDict n s xs = allWordsInDict xs (bestSuggestionN n s xs) 

