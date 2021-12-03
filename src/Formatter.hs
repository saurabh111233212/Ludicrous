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
parseFromText :: Text -> Block
parseFromText text = undefined


-- folds a syntax tree into a formatted string (or block) ,,,, is this needed?? 
formatTree :: Block -> Text
formatTree = undefined



-- | Given a file path, returns the contents of that file as a string
getStringFromFile :: String -> String -- IO String
getStringFromFile fp = undefined

