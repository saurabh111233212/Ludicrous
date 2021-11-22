{--
Formatter to change text into formatted lu code.
--}
module Formatter where


import qualified Parser as P
import Data.Text (Text, pack)
import LuSyntax
import LuParser (parseLuFile)
import Test.HUnit 

-- parser, but parses from Text instead of a String
parseFromText :: Text -> Either P.ParseError Block
parseFromText = undefined

-- folds a syntax tree into a formatted string (or block) 
formatTree :: Block -> Text
formatTree = undefined

-- | Given a file path, returns the contents of that file as a string
getStringFromFile :: String -> String
getStringFromFile fp = undefined

-- | parsing and unparsing should not affect the block 
prop_same_meaning :: Block -> Bool
prop_same_meaning b = parseFromText (formatTree b) == Right b

-- | test Parsing the bfs.lu file
testParseFromText :: Test
testParseFromText = "parsingTest" ~:
                    parseFromText (pack (getStringFromFile "test/bfs.lu")) ~=? Right wBfs
