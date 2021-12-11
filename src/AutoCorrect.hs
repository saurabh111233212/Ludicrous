module AutoCorrect where

import Data.List as L
import LuParser (reserved)
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Data.Text as T
import qualified Data.Vector as V
import Control.Monad.Memo



-- | returns a dictionary with all the words in a given text
initDict :: Text -> [String]
initDict = L.words . unpack


-- | Dictionary containing initial used words (initially only )
initialWords :: [String]
initialWords = []

-- | checks to see if a given word is in the diciontary
isInDict :: String -> [String] -> Bool
isInDict = L.elem 

-- | checks if all words are in the dictionary
allWordsInDict :: [String] -> [String] -> Bool
allWordsInDict dict = L.foldr (\y acc -> y `isInDict` dict && acc) True

-- | adds a word to the dictionary
addWord :: String -> [String] -> [String]
addWord w dict = if w `L.elem` dict then dict else w : dict

deleteWord :: String -> [String] -> [String]
deleteWord w = aux w []
 where
  aux :: String -> [String] -> [String] -> [String]
  aux w rest (word : words) = if w == word then rest L.++ words else aux w (word : rest) words
  aux w rest [] = rest

-- | Finds the distance (Levenshtein) between two strings
distance :: String -> String -> Int
distance s t = startEvalMemo $ distance' ((V.fromList s), (V.fromList t))

-- uses vectors for better runtime 
distance' :: (MonadMemo (V.Vector Char, V.Vector Char) Int m) => (V.Vector Char, V.Vector Char) -> m Int
distance' (s, t) = if V.null s then return $ V.length t else if V.null t then return $ V.length s else
  if V.last s == V.last t then memo distance' ((V.init s), (V.init t)) else do
    d1 <- memo distance' ((V.init s), t)
    d2 <- memo distance' ((V.init s), (V.init t))
    d3 <- memo distance' (s, (V.init t))
    return $ 1 + L.minimum [d1, d2, d3]

-- | Finds the distances between a given string and a list of strings
distances :: String -> [String] -> [(String, Int)]
distances x = L.map (\y -> (y, distance x y))

-- | finds the tuple with the lowest distance, given all (String, distance) pairs
findBest :: [(String, Int)] -> Maybe String
findBest distances = let
    aux :: [(String, Int)] -> Maybe (String, Int) -> Maybe (String, Int)
    aux [] acc = acc

    aux ((s1, d1) : ps) acc = case aux ps acc of 
      Nothing -> Just (s1, d1)
      Just (s2, d2) -> if d2 < d1 then Just (s2, d2) else Just (s1, d1)

  in do
    (s, d) <- aux distances Nothing
    return s


-- | Finds the closest suggestion to a word given a dictionary
bestSuggestion :: String -> [String] -> Maybe String
bestSuggestion word dict = if L.null dict then Nothing else
  let allDistances = distances word dict in
    findBest allDistances


-- | Finds the N closest suggestions to a word given a dictionary
bestSuggestionN :: Int -> String -> [String] -> [String]
bestSuggestionN 0 _ _ = []
bestSuggestionN n word dict = case (bestSuggestion word dict) of 
  Nothing -> []
  Just w -> w : bestSuggestionN (n - 1) word (deleteWord w dict)

