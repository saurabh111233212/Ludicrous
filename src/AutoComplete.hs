module AutoComplete where

import Data.List
import LuParser (reserved)
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))


-- | Dictionary containing initial used words (initially only )
initial_words :: [String]
initial_words = undefined

-- | checks to see if a given word is in the diciontary
isInDict :: [String] -> String -> Bool
isInDict = undefined

-- | checks if all words are in the dictionary
allWordsInDict :: [String] -> [String] -> Bool
allWordsInDict dict = foldr (\y acc -> isInDict dict y && acc) True

-- | adds a word to the dictionary
addWord :: String -> [String] -> [String]
addWord = undefined

-- | Finds the distance (Levenshtein) between two strings
distance :: String -> String -> Int
distance = undefined

-- | Finds the distances between a given string and a list of strings
distances :: String -> [String] -> [(String, Int)]
distances x = map (\y -> (y, distance x y))

-- | Finds the closest suggestion to a word given a dictionary
bestSuggestion :: String -> [String] -> Maybe String
bestSuggestion = undefined

-- | Finds the N closest suggestions to a word given a dictionary
bestSuggestionN :: Int -> String -> [String] -> [String]
bestSuggestionN = undefined

-- test that all suggested words are in the dictionary
prop_allWordsInDict :: Int -> String -> [String] -> Bool
prop_allWordsInDict n s xs = allWordsInDict xs (bestSuggestionN n s xs) 

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
    