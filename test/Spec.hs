import ColorMapperTests
import LuSyntax
import Test.HUnit
import Test.QuickCheck

-- Runs tests
main :: IO ()
main = runColorMapperTests >> putStrLn "Tests Complete"

-- | parsing and unparsing should not affect the block
prop_same_meaning :: Block -> Bool
prop_same_meaning b = parseFromText (formatTree b) == Right b

-- | test Parsing the bfs.lu file
testParseFromText :: Test
testParseFromText =
  "parsingTest"
    ~: parseFromText (pack (getStringFromFile "test/bfs.lu")) ~=? Right wBfs

-- test that all suggested words are in the dictionary
prop_allWordsInDict :: Int -> String -> [String] -> Bool
prop_allWordsInDict n s xs = allWordsInDict xs (bestSuggestionN n s xs)

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
