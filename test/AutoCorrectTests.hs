module AutoCorrectTests where

import AutoCorrect
import Test.HUnit
import qualified Test.QuickCheck as QC
import Data.Set as S


runAutoCorrectTests :: IO ()
runAutoCorrectTests = 
  putStrLn "Distance:" 
  >> runTestTT test_distance
  >> putStrLn "Best Suggestion:"
  >> runTestTT test_bestSuggestion
  >> putStrLn "Best Suggestion N"
  >> runTestTT test_bestSuggestionN
  >> putStrLn "Non negative distance:"
  >> QC.quickCheck prop_nonNegDsitance
  >> putStrLn "Valid Distance:"
  >> QC.quickCheck prop_validDistance
  >> putStrLn "All words in Dict:"
  >> QC.quickCheck prop_allWordsInDict
  >> putStrLn "AutoCorrect Tests Complete"



-- tests the lvenstein distance function
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
            [ bestSuggestion "a" empty ~?= Nothing,
              bestSuggestion "a" (S.fromList ["b"]) ~?= Just "b",
              bestSuggestion "a" (S.fromList ["a"]) ~?= Just "a",
              bestSuggestion "a" (S.fromList ["a", "ab"]) ~?= Just "a"
            ]

-- | Tests the multiple suggestions function
test_bestSuggestionN :: Test
test_bestSuggestionN = 
  "bestSuggestionN"
    ~: TestList
      [
        bestSuggestionN 4 "a" empty ~?= [],
        bestSuggestionN 23 "a" (S.fromList ["b"]) ~?= ["b"],
        bestSuggestionN 1 "a" (S.fromList ["a"]) ~?= ["a"],
        bestSuggestionN 2 "a" (S.fromList ["a", "ab"]) ~?= ["a", "ab"],
        bestSuggestionN 3 "abd" (S.fromList ["a", "ab", "abd", "aaaaaa", "aaaa"])~?= ["abd", "ab", "a"]
      ]


-- test that all suggested words are in the dictionary
prop_allWordsInDict :: Int -> String -> S.Set String-> Bool
prop_allWordsInDict n s xs = allWordsInDict xs (bestSuggestionN n s xs) 

-- | the distnace should never be negative
prop_nonNegDsitance :: String -> String -> Bool
prop_nonNegDsitance s t = distance s t >= 0

-- | the distance should never exceed the length or either string
prop_validDistance :: String -> String -> Bool
prop_validDistance s t = let d = distance s t in d <= length s || d <= length t