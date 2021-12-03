module AutoComplete where

import Data.List
import LuParser (reserved)
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))


-- | Dictionary containing initial used words (initially only )
initialWords :: [String]
initialWords = []

-- | checks to see if a given word is in the diciontary
isInDict :: String -> [String] -> Bool
isInDict = elem 

-- | checks if all words are in the dictionary
allWordsInDict :: [String] -> [String] -> Bool
allWordsInDict dict = foldr (\y acc -> y `isInDict` dict && acc) True

-- | adds a word to the dictionary
addWord :: String -> [String] -> [String]
addWord w dict = if w `elem` dict then dict else w : dict

deleteWord :: String -> [String] -> [String]
deleteWord w = aux w []
 where
  aux :: String -> [String] -> [String] -> [String]
  aux w rest (word : words) = if w == word then rest ++ words else aux w (word : rest) words
  aux w rest [] = rest

-- | Finds the distance (Levenshtein) between two strings
distance :: String -> String -> Int
distance "" "" = 0
distance xs "" = length xs
distance "" ys = length ys
distance (x : xs) (y : ys) = if x == y then distance xs ys else 
  minimum [
            1 + distance (x : xs) (ys), 
            1 + distance (xs) (y : ys),
            1 + distance xs ys
          ]

-- | Finds the distances between a given string and a list of strings
distances :: String -> [String] -> [(String, Int)]
distances x = map (\y -> (y, distance x y))

findBest :: [(String, Int)] -> Maybe String
findBest distances = let     
    aux [] Nothing = Nothing
    aux ((s1, d1) : ps) Nothing = case aux ps (Just d1) of 
      Nothing -> Just (s1, d1)
      Just (s2, d2) -> if d2 < d1 then Just (s2, d2) else Just (s1, d1)
  in do
    (s, d) <- aux distances Nothing
    return s


-- | Finds the closest suggestion to a word given a dictionary
bestSuggestion :: String -> [String] -> Maybe String
bestSuggestion word dict = if null dict then Nothing else
  let allDistances = distances word dict in
    findBest allDistances


-- | Finds the N closest suggestions to a word given a dictionary
bestSuggestionN :: Int -> String -> [String] -> [String]
bestSuggestionN 0 _ _ = []
bestSuggestionN n word dict = case (bestSuggestion word dict) of 
  Nothing -> []
  Just w -> w : bestSuggestionN (n - 1) word (deleteWord w dict)


