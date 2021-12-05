{--
Formatter to change text into formatted lu code.
--}
module Formatter where


import qualified Parser as P
import Data.Text (Text, pack)
import LuSyntax
import LuParser (parseLuFile, blockP)
import Test.HUnit hiding (Path)
import qualified Data.Text as T
import Path
import Path.IO
import Data.List.Split (splitOn)
import Data.Text.IO
import Parser


-- | parses from Data.Text into a block
parseFromText :: Text -> Either ParseError Block
parseFromText t = let parser = (const <$> blockP <*> P.eof) in parse parser (T.unpack t)

-- | takes a block and converts it to formatted Text
formatBlock :: Block -> Text
formatBlock = pack . pretty 


