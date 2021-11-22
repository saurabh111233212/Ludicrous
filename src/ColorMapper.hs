module ColorMapper where

import LuParser (reserved)


newtype Color = C {color :: String} deriving (Eq, Show)

-- | List of reserved key words
keyWords :: [String]
keyWords = reserved

-- explicit mapping from a string to its Color
colorMap :: String -> (String, Color)
colorMap = undefined

-- | colors a whole sentence, represented as a list of words
colorMapSentence :: [String] -> [(String, Color)]
colorMapSentence = undefined 

-- | checks the pre and post colored sentence to make sure it's the same. 
sameWords :: [String] -> [(String, Color)] -> Bool
sameWords  = undefined 

-- | test that the coloring of words does not affect the meaning of the sentence
prop_wordsUnaffected :: [String] -> Bool
prop_wordsUnaffected s = sameWords s (colorMapSentence s)

-- | checks to make sure keyWords are colored
checkKeyWords :: [(String, Color)] -> Bool
checkKeyWords = undefined

-- | tests that coloring does color all the keywords
prop_keyWordsColored :: [String] -> Bool
prop_keyWordsColored s = let coloredS  = colorMapSentence s in
                        checkKeyWords coloredS

