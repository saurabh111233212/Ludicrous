module ColorMapper where

import LuParser (reserved)


newtype Color = C {color :: String} deriving (Eq, Show)

-- | List of reserved key words
keyWords :: [String]
keyWords = reserved

-- explicit mapping from a string to its Color
colorMap :: String -> (String, Color)
colorMap s = if s `elem` keyWords then (s, C "Orange") else (s, C "White")

-- | colors a whole sentence, represented as a list of words
colorMapSentence :: [String] -> [(String, Color)]
colorMapSentence l = colorMap <$> l

