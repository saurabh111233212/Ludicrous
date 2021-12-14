module ColorMapper
  ( Color (C),
    colorMap,
    attribute,
    wsParser,
    stringParser,
    charParser,
    keyParser,
    opParser,
    miscParser,
  )
where

import Data.Functor
import Data.Text (Text, pack)
import qualified Data.Text as T
import LuParser (reserved)
import Text.Parsec
import Text.Parsec.Error
import Text.Parsec.Prim
import Text.Parsec.Text

newtype Color = C {attribute :: String} deriving (Eq, Show)

-- | List of reserved key words
keyWords :: [String]
keyWords = LuParser.reserved

-- | explicit mapping from a string to its Color
colorMap :: Text -> [(Text, Color)]
colorMap s = case runParser lineParser () "" s of
  Left e -> error (concatMap messageString (errorMessages e))
  Right x -> x

-- | parser for a line
lineParser :: Stream s m Char => ParsecT s u m [(Text, Color)]
lineParser =
  many1
    ( try stringParser
        <|> try charParser
        <|> try opParser
        <|> try wsParser
        <|> try keyParser
        <|> try miscParser
    )
    <|> pure [(pack " ", C "text")]

-- | parser for spaces and other whitespace
wsParser :: Stream s m Char => ParsecT s u m (Text, Color)
wsParser = do
  x <- many1 (oneOf " \t")
  return (pack x, C "whitespace")

-- | parses string literals
stringParser :: Stream s m Char => ParsecT s u m (Text, Color)
stringParser = do
  x <- char '\"' *> many (noneOf "'\"\n'") <* char '\"'
  return (pack ('\"' : x ++ ['\"']), C "string")

-- | parses character literals
charParser :: Stream s m Char => ParsecT s u m (Text, Color)
charParser = do
  x <- char '\'' *> noneOf "'\"\n" <* char '\''
  return (pack ('\'' : x : ['\'']), C "string")

-- | parses keywords
keyParser :: Stream s m Char => ParsecT s u m (Text, Color)
keyParser = do
  x <-
    choice
      ( map
          (try . string)
          ["if", "then", "else", "repeat", "until", "while", "do", "end"]
      )
      <* lookAhead (choice [wsParser, (T.empty, C "") Data.Functor.<$ eof])
  return (pack x, C "keyword")

-- | parses operators
opParser :: Stream s m Char => ParsecT s u m (Text, Color)
opParser = do
  x <-
    choice $
      map
        (try . string)
        [ "+",
          "-",
          "*",
          "//",
          "/",
          "%",
          "==",
          ">=",
          "<=",
          "=",
          "<",
          ">",
          "..",
          ".",
          "not",
          "{",
          "}",
          "[",
          "]",
          "#",
          ";"
        ]
  return (pack x, C "operator")

-- | parses everything else
miscParser :: Stream s m Char => ParsecT s u m (Text, Color)
miscParser = do
  x <- many1 (noneOf " +-*{}[]=-><%#/.;\n")
  return (pack x, C "text")
