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
    Left e -> error (concatMap messageString (errorMessages e))
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
    x <- many1 (oneOf " \t")
    return (pack x, C "whitespace")

escape = do
    d <- char '\\'
    c <- oneOf "\\\"0nrvtbf" -- all the characters which can be escaped
    return [d, c]

nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"

character = fmap return nonEscape <|> escape

-- parses string literals
stringParser :: Stream s m Char => ParsecT s u m (Text, Color)
stringParser = do
    x <- char '\"' *> many (noneOf "'\"\n'") <* char '\"'
    return (pack ('\"' : x ++ ['\"']), C "string")

-- parses character literals
charParser :: Stream s m Char => ParsecT s u m (Text, Color)
charParser = do
    x <- char '\'' *> many (noneOf "'\"\n'") <* char '\''
    return (pack ('\'' : x ++ ['\'']), C "string")

-- parses keywords
keyParser :: Stream s m Char => ParsecT s u m (Text, Color)
keyParser = do
    x <- (choice $ map string ["if", "then", "else", "repeat", "until", "while", "do", "end"]) <* lookAhead (choice [wsParser, pure (T.empty, C $ "") <* eof])
    return (pack x, C "keyword")

-- parses operators
opParser :: Stream s m Char => ParsecT s u m (Text, Color)
opParser = do
    x <- choice $ map string ["+", "-", "*", "/", "%", "=", "<", ">", ".", "not", "{", "}", "[", "]", "#", ";"]
    return (pack x, C "operator")

-- parses everything else
miscParser :: Stream s m Char => ParsecT s u m (Text, Color)
miscParser = do
    x <- many1 (noneOf " +-*{}[]=-><%#/.;\n")
    return (pack x, C "text")
