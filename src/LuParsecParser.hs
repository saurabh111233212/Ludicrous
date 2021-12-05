{-
A Parser for Lu
===============
-}

{-# LANGUAGE ScopedTypeVariables #-}

module LuParsecParser where

import Control.Applicative hiding ((<|>), many)
import qualified Data.Char as Char
import LuSyntax hiding (Empty)
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import qualified Test.QuickCheck as QC
import Control.Monad (guard)


import Text.Parsec hiding (Empty)
import Text.Parsec.Error
import Text.Parsec.Prim
import Text.Parsec.Text
import Text.Parsec.Pos
import Text.Parsec.Char
import Text.Megaparsec.Byte.Lexer as L hiding (space)

import Prelude hiding (filter)



-- | Filter the parsing results by a predicate
filter :: Stream s m Char => (a -> Bool) -> ParsecT s u m a -> ParsecT s u m a
filter f p = undefined 

{-P $ \s ->  do 
                         (c , cs) <- parse p s
                         guard (f c)
                         return (c , cs)
-}


{-
Testing your Parser
------------------
-}


prop_roundtrip_val :: Value -> Bool
prop_roundtrip_val v = parse valueP "" (pretty v) == Right v

prop_roundtrip_exp :: Expression -> Bool
prop_roundtrip_exp e = parse expP "" (pretty e) == Right e

prop_roundtrip_stat :: Statement -> Bool
prop_roundtrip_stat s = parse statementP "" (pretty s) == Right s


wsP :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
wsP p = p <* many space

test_wsP :: Test
test_wsP =
  TestList
    [ parse (wsP letter) "" "a" ~?= Right 'a',
      parse (many (wsP letter)) "" "a b \n   \t c" ~?= Right "abc"
    ]

-- >>> runTestTT test_wsP                    
-- Cases: 2  Tried: 2  Errors: 0  Failures: 0
-- Counts {cases = 2, tried = 2, errors = 0, failures = 0}
--

stringP :: Stream s m Char => String -> ParsecT s u m ()
stringP s = () <$ wsP (string s)


test_stringP :: Test
test_stringP =
  TestList
    [ parse (stringP "a") "" "a" ~?= Right (),
      parse (stringP "a") "" "b" ~?= Left (mergeError (newErrorMessage (UnExpect "\"b\"") (initialPos "")) (newErrorMessage (Expect "\"a\"") (initialPos ""))),
      parse (many (stringP "a")) "" "a  a" ~?= Right [(), ()]
    ]


-- >>> runTestTT test_stringP                
-- Cases: 3  Tried: 3  Errors: 0  Failures: 0
-- Counts {cases = 3, tried = 3, errors = 0, failures = 0}
--


{-
Define a parser that will accept a particular string `s`, returning a
given value `x`, and also and consume any white space that follows.
-}

constP :: Stream s m Char => String -> a -> ParsecT s u m a
constP s a = a <$ stringP s


test_constP :: Test
test_constP =
  TestList
    [ parse (constP "&" 'a') "" "&  " ~?= Right 'a',
      parse (many (constP "&" 'a')) "" "&   &" ~?= Right "aa"
    ]

-- >>> runTestTT test_constP                                   
-- Cases: 2  Tried: 2  Errors: 0  Failures: 0
-- Counts {cases = 2, tried = 2, errors = 0, failures = 0}
--


{-
We will also use `stringP` for some useful operations that parse between
delimiters, consuming additional whitespace.
-}

parens :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
parens = between (stringP "(") (stringP ")")

braces :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
braces = between (stringP "{") (stringP "}")


-- >>> parse (many (brackets (constP "1" 1))) "" "[1] [  1]   [1 ]"
-- Right [1]

brackets :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
brackets = (between (char '[') (char ']'))

-- parses between quotes
quotes :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
quotes = between (char '\"') (char '\"')

{-
Parsing Constants
-----------------

Now let's write parsers for the `Value` type, except for table constants
(which we won't parse).
-}

valueP :: Stream s m Char => ParsecT s u m Value
valueP = intValP <|> boolValP <|> nilValP <|> stringValP


{-
To do so, fill in the implementation of the four parsers above. As above, these
four parsers should consume any following whitespace. You can make sure that happens
by testing 'many' uses of the parser in a row.
-}

-- >>> parse (many intValP) "" "1 2\n 3"
-- Right [IntVal 1,IntVal 2,IntVal 3]
int :: (Read b, Stream s m Char) => ParsecT s u m b
int = read <$> ((++) <$> string "-" <*> some digit <|> some digit)

intValP :: Stream s m Char => ParsecT s u m Value
intValP = IntVal <$> wsP int

-- >>> parse (many boolValP) "" "true false\n true"
-- Right [BoolVal True,BoolVal False,BoolVal True]
boolValP :: Stream s m Char => ParsecT s u m Value
boolValP = BoolVal <$> (constP "true" True <|> constP "false" False)

-- >>> parse (many nilValP) "" "nil nil\n nil"
-- Right [NilVal,NilVal,NilVal]
nilValP :: Stream s m Char => ParsecT s u m Value
nilValP = constP "nil" NilVal

{-
Lu literal strings are sequences of non-quote characters between double
quotes. For simplicity, Lu does not allow escaped quote characters to appear
in literal strings.
-}

stringValP :: Stream s m Char => ParsecT s u m Value
stringValP = StringVal <$> wsP (quotes (many $ satisfy (/= '\"')))


test_stringValP :: Test
test_stringValP =
  TestList
    [ parse stringValP "" "\"a\"" ~?= Right (StringVal "a"),
      parse stringValP "" "\"a\\\"\"" ~?= Right (StringVal "a\\"),
      parse (many stringValP) "" "\"a\"   \"b\"" ~?= Right [StringVal "a", StringVal "b"],
      parse (many stringValP) "" "\" a\"   \"b\"" ~?= Right [StringVal " a", StringVal "b"]
    ]


-- >>> runTestTT test_stringValP             
-- Cases: 4  Tried: 4  Errors: 0  Failures: 0
-- Counts {cases = 4, tried = 4, errors = 0, failures = 0}
--

{-
At this point you should be able to quickcheck the `prop_roundtrip_val` property. You'll need to do this
in the terminal.

   > QC.quickCheck prop_roundtrip_val

Parsing Expressions
-------------------

Next, let's parse some Lu expressions.

We've already stratified the grammar for you, so that we'll get the
appropriate precedence and associativity for the binary and unary
operators. Make sure to read the end of the parsers lecture to understand how
this code works.

However, this code *won't* work until you complete all the parts of this section.
-}

--expP :: Parser Expression
expP :: Stream s m Char => ParsecT s u m Expression
expP = compP
  where
    compP = catP `chainl1` opAtLevel (level Gt)
    catP = sumP `chainl1` opAtLevel (level Concat)
    sumP = prodP `chainl1` opAtLevel (level Plus)
    prodP = uopexpP `chainl1` opAtLevel (level Times)
    uopexpP =
      baseP
        <|> Op1 <$> uopP <*> uopexpP
    baseP =
      tableConstP
        <|> Var <$> varP
        <|> parens expP
        <|> Val <$> valueP


-- | Parse an operator at a specified precedence level
--opAtLevel :: Int -> Parser (Expression -> Expression -> Expression)
opAtLevel :: Stream s m Char => Int -> ParsecT s u m (Expression -> Expression -> Expression)
opAtLevel l = flip Op2 <$> filter (\x -> level x == l) bopP

{-
A variable is a prefix followed by some number of indexing terms or just a name.
We've also done this one for you.
-}

-- >>>  parse (many varP) "x y z"
-- Right [Name "x",Name "y",Name "z"]
-- >>> parse varP "(x.y[1]).z"
-- Right (Dot (Var (Proj (Var (Dot (Var (Name "x")) "y")) (Val (IntVal 1)))) "z")
--varP :: Parser Var
varP :: ParsecT s u m Var
varP = undefined {-mkVar <$> prefixP <*> some indexP <|> Name <$> nameP
  where
    mkVar :: Expression -> [Expression -> Var] -> Var
    mkVar e l = foldr1 (\f p u -> p (Var (f u))) l e

    --prefixP :: Parser Expression
    prefixP = parens expP <|> Var . Name <$> nameP

    --indexP :: Parser (Expression -> Var)
    indexP =
      flip Dot <$> (string "." *> nameP)
        <|> flip Proj <$> brackets expP
-}

{-
Define an expression parser for names. Names can be any sequence of upper and
lowercase letters, digits and underscores, not beginning with a digit and not
being a reserved word. Your parser should also consume any trailing
whitespace characters.
-}

reserved :: [String]
reserved =
  [ "and",
    "break",
    "do",
    "else",
    "elseif",
    "end",
    "false",
    "for",
    "function",
    "goto",
    "if",
    "in",
    "local",
    "nil",
    "not",
    "or",
    "repeat",
    "return",
    "then",
    "true",
    "until",
    "while"
  ]

-- >>> parse (many nameP) "x sfds _ nil"
-- Right ["x","sfds","_"]
--nameP :: Parser Name
nameP :: ParsecT s u m Name
nameP = undefined --filter (`notElem` reserved) (wsP $ (++) <$> some (letter <|> upper <|> lower <|> satisfy (== '_')) <*> many (letter <|> digit <|> upper <|> lower <|> satisfy (== '_')))

{-
Now write parsers for the unary and binary operators. Make sure you check out the [manual](LuManual.md)
or the LuSyntax module for the list of all possible operators. The tests are not exhaustive.
-}

-- >>> parse (many uopP) "- - #"
-- Right [Neg,Neg,Len]
--uopP :: Parser Uop
uopP =
  wsP
    ( stringP "-" *> pure Neg
        <|> stringP "not" *> pure Not
        <|> stringP "#" *> pure Len
    )

-- >>> parse (many bopP) "+ >= .."
-- Right [Plus,Gt]
--bopP :: Parser Bop
bopP =
  wsP
    ( stringP "+" *> pure Plus
        <|> stringP "-" *> pure Minus
        <|> stringP "*" *> pure Times
        <|> stringP "//" *> pure Divide
        <|> stringP "%" *> pure Modulo
        <|> stringP "==" *> pure Eq
        <|> stringP ">=" *> pure Ge
        <|> stringP ">" *> pure Gt
        <|> stringP "<=" *> pure Le
        <|> stringP "<" *> pure Lt
        <|> stringP ".." *> pure Concat
    )

{-
Finally write a parser for table construction:
-}

-- >>> parse tableConstP "{ x = 2, [3] = false }"
-- Right (TableConst [FieldName "x" (Val (IntVal 2)),FieldKey (Val (IntVal 3)) (Val (BoolVal False))])

--tableConstP :: Parser Expression
tableConstP = undefined {-
  TableConst
    <$> braces
      ( sepBy
          ( (FieldName <$> nameP <* stringP "=" <*> expP)
              <|> (FieldKey <$> brackets expP <* stringP "=" <*> expP)
          )
          (stringP ",")
      )
-}
{-
At this point you should be able to quickcheck the `prop_roundtrip_exp` property.

Parsing Statements
------------------

Finally, define a parser for statements ...
-}

--ifP :: Parser Statement
ifP = undefined {-
  If <$> (stringP "if" *> expP)
    <*> (stringP "then" *> blockP)
    <*> (constP "end" (Block []) <|> between (stringP "else") blockP (stringP "end"))
-}
--assignP :: Parser Statement
assignP = Assign <$> varP <* stringP "=" <*> expP

--whileP :: Parser Statement
whileP = undefined-- While <$> (stringP "while" *> expP) <*> between (stringP "do") blockP (stringP "end")

-- emptyP :: Parser Statement
emptyP = constP ";" Empty

-- repeatP :: Parser Statement
repeatP = Repeat <$> (stringP "repeat" *> blockP) <*> (stringP "until" *> expP)

-- statementP :: Parser Statement
statementP = undefined --emptyP <|> ifP <|> whileP <|> repeatP <|> assignP

{-
... and one for blocks.
-}

--blockP :: Parser Block
blockP = Block <$> many statementP

{-
At this point you should be able to quickcheck the `prop_roundtrip_stat` property.

Parsing Expressions and Files
-----------------------------
-}

--parseLuExp :: String -> Either ParseError Expression
parseLuExp = parse expP

--parseLuStat :: String -> Either ParseError Statement
parseLuStat = parse statementP

-- parseLuFile :: String -> IO (Either ParseError Block)
parseLuFile = undefined --parseFromFile (const <$> blockP <*> eof)

{-
Unit Tests
---------

These unit tests summarize the tests given above.


test_comb =
  "parsing combinators"
    ~: TestList
      [ parse (wsP letter) "a" ~?= Right 'a',
        parse (many (wsP letter)) "a b \n   \t c" ~?= Right "abc",
        parse (stringP "a") "a" ~?= Right (),
        parse (stringP "a") "b" ~?= Left "No parses",
        parse (many (stringP "a")) "a  a" ~?= Right [(), ()],
        parse (constP "&" 'a') "&  " ~?= Right 'a',
        parse (many (constP "&" 'a')) "&   &" ~?= Right "aa",
        parse (many (brackets (constP "1" 1))) "[1] [  1]   [1 ]" ~?= Right [1, 1, 1]
      ]

test_value =
  "parsing values"
    ~: TestList
      [ parse (many intValP) "1 2\n 3" ~?= Right [IntVal 1, IntVal 2, IntVal 3],
        parse (many boolValP) "true false\n true" ~?= Right [BoolVal True, BoolVal False, BoolVal True],
        parse (many nilValP) "nil nil\n nil" ~?= Right [NilVal, NilVal, NilVal],
        parse stringValP "\"a\"" ~?= Right (StringVal "a"),
        parse stringValP "\"a\\\"\"" ~?= Right (StringVal "a\\"),
        parse (many stringValP) "\"a\"   \"b\"" ~?= Right [StringVal "a", StringVal "b"],
        parse (many stringValP) "\" a\"   \"b\"" ~?= Right [StringVal " a", StringVal "b"]
      ]

test_exp =
  "parsing expressions"
    ~: TestList
      [ parse (many varP) "x y z" ~?= Right [Name "x", Name "y", Name "z"],
        parse varP "(x.y[1]).z" ~?= Right (Dot (Var (Proj (Var (Dot (Var (Name "x")) "y")) (Val (IntVal 1)))) "z"),
        parse (many nameP) "x sfds _ nil" ~?= Right ["x", "sfds", "_"],
        parse (many uopP) "- - #" ~?= Right [Neg, Neg, Len],
        parse (many bopP) "+ >= .." ~?= Right [Plus, Ge, Concat],
        parse tableConstP "{ x = 2, [3] = false }"
          ~?= Right (TableConst [FieldName "x" (Val (IntVal 2)), FieldKey (Val (IntVal 3)) (Val (BoolVal False))])
      ]

test_stat =
  "parsing statements"
    ~: TestList
      [ parse statementP ";" ~?= Right Empty,
        parse statementP "x=3" ~?= Right (Assign (Name "x") (Val (IntVal 3))),
        parse statementP "if x then y=nil else end"
          ~?= Right (If (Var (Name "x")) (Block [Assign (Name "y") (Val NilVal)]) (Block [])),
        parse statementP "while nil do end"
          ~?= Right (While (Val NilVal) (Block [])),
        parse statementP "repeat ; ; until false"
          ~?= Right (Repeat (Block [Empty, Empty]) (Val (BoolVal False)))
      ]
-}
-- >>> runTestTT test_stat

{-
Testing summary
---------------
-}

--test_all :: IO Counts
--test_all = runTestTT $ TestList [test_comb, test_value, test_exp, test_stat]

-- >>> test_all
{-
qc :: IO ()
qc = do
  putStrLn "roundtrip_val"
  QC.quickCheck prop_roundtrip_val
  putStrLn "roundtrip_exp"
  QC.quickCheck prop_roundtrip_exp
  putStrLn "roundtrip_stat"
  QC.quickCheck prop_roundtrip_stat

-}
