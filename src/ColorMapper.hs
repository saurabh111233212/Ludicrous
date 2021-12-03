module ColorMapper where

import LuParser (reserved)
import Data.Text (Text, pack)
import qualified Data.Text as T

import Text.Parsec
import Text.Parsec.Error
import Text.Parsec.Prim
import Text.Parsec.Text
-- import Text.Parsec.Token


newtype Color = C {attribute :: String} deriving (Eq, Show)

-- | List of reserved key words
keyWords :: [String]
keyWords = LuParser.reserved

-- explicit mapping from a string to its Color
colorMap :: Text -> [(Text, Color)]
colorMap s = case runParser lineParser () "" s of
    Left e -> error (concat $ map messageString (errorMessages e))
    Right x -> x

-- parser for a line
lineParser :: Stream s m Char => ParsecT s u m [(Text, Color)]
lineParser =  many1 ( try stringParser <|>
                      try charParser <|>
                        try opParser <|>
                        try wsParser <|>
                        try keyParser <|>
                        try miscParser
                        ) <|> pure [(pack " ", C "text")]

-- parser for spaces
wsParser :: Stream s m Char => ParsecT s u m (Text, Color)
wsParser = do
    x <- many1 (char ' ')
    return (pack x, C "whitespace")

-- parses string literals
stringParser :: Stream s m Char => ParsecT s u m (Text, Color)
stringParser = do
    x <- (char '\"') *> many (noneOf "'\"\n'") <* (char '\"')
    return (pack ('\"' : x ++ ['\"']), C "string")

-- parses character literals
charParser :: Stream s m Char => ParsecT s u m (Text, Color)
charParser = do
    x <- (char '\'') *> many (noneOf "'\"\n'") <* (char '\'')
    return (pack ('\'' : x ++ ['\'']), C "string")

-- parses keywords
keyParser :: Stream s m Char => ParsecT s u m (Text, Color)
keyParser = do
    x <- string "if"
        <|> string "then"
        <|> string "else"
        <|> string "repeat"
        <|> string "until"
        <|> string "while"
        <|> string "do"
        <|> string "end"
    return (pack x, C "keyword")

-- parses operators
opParser :: Stream s m Char => ParsecT s u m (Text, Color)
opParser = do
    x <- string "+"
        <|> string "-"
        <|> string "*"
        <|> string "/"
        <|> string "%"
        <|> string "="
        <|> string "="
        <|> string ">"
        <|> string "<"
        <|> string ".."
        <|> string "."
        <|> string "not"
        <|> string "{"
        <|> string "}"
        <|> string "["
        <|> string "]"
        <|> string "#"
        <|> string ";"
    return $ (pack x, C "operator")

-- parses everything else
miscParser :: Stream s m Char => ParsecT s u m (Text, Color)
miscParser = do
    x <- many1 (noneOf " +-*{}[]=-><%#/;\n")
    return $ (pack x, C "text")
