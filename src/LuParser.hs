{-
A Parser for Lu
===============
-}
{-# LANGUAGE ScopedTypeVariables #-}

module LuParser where

import Control.Applicative
import qualified Data.Char as Char
import LuSyntax
import Parser (Parser)
import qualified Parser as P
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import qualified Test.QuickCheck as QC

{-
Testing your Parser
------------------
-}

prop_roundtrip_val :: Value -> Bool
prop_roundtrip_val v = P.parse valueP (pretty v) == Right v

prop_roundtrip_exp :: Expression -> Bool
prop_roundtrip_exp e = P.parse expP (pretty e) == Right e

prop_roundtrip_stat :: Statement -> Bool
prop_roundtrip_stat s = P.parse statementP (pretty s) == Right s

wsP :: Parser a -> Parser a
wsP p = p <* many P.space

test_wsP :: Test
test_wsP =
  TestList
    [ P.parse (wsP P.alpha) "a" ~?= Right 'a',
      P.parse (many (wsP P.alpha)) "a b \n   \t c" ~?= Right "abc"
    ]

-- >>> runTestTT test_wsP
-- Counts {cases = 2, tried = 2, errors = 0, failures = 0}

stringP :: String -> Parser ()
stringP s = () <$ wsP (P.string s)

test_stringP :: Test
test_stringP =
  TestList
    [ P.parse (stringP "a") "a" ~?= Right (),
      P.parse (stringP "a") "b" ~?= Left "No parses",
      P.parse (many (stringP "a")) "a  a" ~?= Right [(), ()]
    ]

-- >>> runTestTT test_stringP
-- Counts {cases = 3, tried = 3, errors = 0, failures = 0}

{-
Define a parser that will accept a particular string `s`, returning a
given value `x`, and also and consume any white space that follows.
-}

constP :: String -> a -> Parser a
constP s a = a <$ stringP s

test_constP :: Test
test_constP =
  TestList
    [ P.parse (constP "&" 'a') "&  " ~?= Right 'a',
      P.parse (many (constP "&" 'a')) "&   &" ~?= Right "aa"
    ]

-- >>> runTestTT test_constP

{-
We will also use `stringP` for some useful operations that parse between
delimiters, consuming additional whitespace.
-}

parens :: Parser a -> Parser a
parens x = P.between (stringP "(") x (stringP ")")

braces :: Parser a -> Parser a
braces x = P.between (stringP "{") x (stringP "}")

-- >>> P.parse (many (brackets (constP "1" 1))) "[1] [  1]   [1 ]"
-- Right [1,1,1]
--
brackets :: Parser a -> Parser a
brackets x = P.between (stringP "[") x (stringP "]")

-- parses between quotes
quotes :: Parser a -> Parser a
quotes x = P.between (P.char '\"') x (P.char '\"')

{-
Parsing Constants
-----------------

Now let's write parsers for the `Value` type, except for table constants
(which we won't parse).
-}

valueP :: Parser Value
valueP = intValP <|> boolValP <|> nilValP <|> stringValP

{-
To do so, fill in the implementation of the four parsers above. As above, these
four parsers should consume any following whitespace. You can make sure that happens
by testing 'many' uses of the parser in a row.
-}

-- >>> P.parse (many intValP) "1 2\n 3"
-- Right [IntVal 1,IntVal 2,IntVal 3]
intValP :: Parser Value
intValP = IntVal <$> wsP P.int

-- >>> P.parse (many boolValP) "true false\n true"
-- Right [BoolVal True,BoolVal False,BoolVal True]
boolValP :: Parser Value
boolValP = BoolVal <$> (constP "true" True <|> constP "false" False)

-- >>> P.parse (many nilValP) "nil nil\n nil"
-- Right [NilVal,NilVal,NilVal]
nilValP :: Parser Value
nilValP = constP "nil" NilVal

{-
Lu literal strings are sequences of non-quote characters between double
quotes. For simplicity, Lu does not allow escaped quote characters to appear
in literal strings.
-}

stringValP :: Parser Value
stringValP = StringVal <$> wsP (quotes (many $ P.satisfy (/= '\"')))

test_stringValP :: Test
test_stringValP =
  TestList
    [ P.parse stringValP "\"a\"" ~?= Right (StringVal "a"),
      P.parse stringValP "\"a\\\"\"" ~?= Right (StringVal "a\\"),
      P.parse (many stringValP) "\"a\"   \"b\"" ~?= Right [StringVal "a", StringVal "b"],
      P.parse (many stringValP) "\" a\"   \"b\"" ~?= Right [StringVal " a", StringVal "b"]
    ]

-- >>> runTestTT test_stringValP
-- Counts {cases = 4, tried = 4, errors = 0, failures = 0}

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

expP :: Parser Expression
expP = compP
  where
    compP = catP `P.chainl1` opAtLevel (level Gt)
    catP = sumP `P.chainl1` opAtLevel (level Concat)
    sumP = prodP `P.chainl1` opAtLevel (level Plus)
    prodP = uopexpP `P.chainl1` opAtLevel (level Times)
    uopexpP =
      baseP
        <|> Op1 <$> uopP <*> uopexpP
    baseP =
      tableConstP
        <|> Var <$> varP
        <|> parens expP
        <|> Val <$> valueP

-- | Parse an operator at a specified precedence level
opAtLevel :: Int -> Parser (Expression -> Expression -> Expression)
opAtLevel l = flip Op2 <$> P.filter (\x -> level x == l) bopP

{-
A variable is a prefix followed by some number of indexing terms or just a name.
We've also done this one for you.
-}

-- >>>  P.parse (many varP) "x y z"
-- Right [Name "x",Name "y",Name "z"]
-- >>> P.parse varP "(x.y[1]).z"
-- Right (Dot (Var (Proj (Var (Dot (Var (Name "x")) "y")) (Val (IntVal 1)))) "z")
varP :: Parser Var
varP = mkVar <$> prefixP <*> some indexP <|> Name <$> nameP
  where
    mkVar :: Expression -> [Expression -> Var] -> Var
    mkVar e l = foldr1 (\f p u -> p (Var (f u))) l e

    prefixP :: Parser Expression
    prefixP = parens expP <|> Var . Name <$> nameP

    indexP :: Parser (Expression -> Var)
    indexP =
      flip Dot <$> (P.string "." *> nameP)
        <|> flip Proj <$> brackets expP

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

-- >>> P.parse (many nameP) "x sfds _ nil"
-- Right ["x","sfds","_"]
nameP :: Parser Name
nameP = P.filter (`notElem` reserved) (wsP $ (++) <$> some (P.alpha <|> P.upper <|> P.lower <|> P.satisfy (== '_')) <*> many (P.alpha <|> P.digit <|> P.upper <|> P.lower <|> P.satisfy (== '_')))

{-
Now write parsers for the unary and binary operators. Make sure you check out the [manual](LuManual.md)
or the LuSyntax module for the list of all possible operators. The tests are not exhaustive.
-}

-- >>> P.parse (many uopP) "- - #"
-- Right [Neg,Neg,Len]
uopP :: Parser Uop
uopP =
  wsP
    ( stringP "-" *> pure Neg
        <|> stringP "not" *> pure Not
        <|> stringP "#" *> pure Len
    )

-- >>> P.parse (many bopP) "+ >= .."
-- Right [Plus,Gt]
bopP :: Parser Bop
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

-- >>> P.parse tableConstP "{ x = 2, [3] = false }"
-- Right (TableConst [FieldName "x" (Val (IntVal 2)),FieldKey (Val (IntVal 3)) (Val (BoolVal False))])
tableConstP :: Parser Expression
tableConstP =
  TableConst
    <$> braces
      ( P.sepBy
          ( (FieldName <$> nameP <* stringP "=" <*> expP)
              <|> (FieldKey <$> brackets expP <* stringP "=" <*> expP)
          )
          (stringP ",")
      )

{-
At this point you should be able to quickcheck the `prop_roundtrip_exp` property.

Parsing Statements
------------------

Finally, define a parser for statements ...
-}

ifP :: Parser Statement
ifP =
  If <$> (stringP "if" *> expP)
    <*> (stringP "then" *> blockP)
    <*> (constP "end" (Block []) <|> P.between (stringP "else") blockP (stringP "end"))

assignP :: Parser Statement
assignP = Assign <$> varP <* stringP "=" <*> expP

whileP :: Parser Statement
whileP = While <$> (stringP "while" *> expP) <*> P.between (stringP "do") blockP (stringP "end")

emptyP :: Parser Statement
emptyP = constP ";" Empty

repeatP :: Parser Statement
repeatP = Repeat <$> (stringP "repeat" *> blockP) <*> (stringP "until" *> expP)

statementP :: Parser Statement
statementP = emptyP <|> ifP <|> whileP <|> repeatP <|> assignP

{-
... and one for blocks.
-}

blockP :: Parser Block
blockP = Block <$> many statementP

{-
At this point you should be able to quickcheck the `prop_roundtrip_stat` property.

Parsing Expressions and Files
-----------------------------
-}

parseLuExp :: String -> Either P.ParseError Expression
parseLuExp = P.parse expP

parseLuStat :: String -> Either P.ParseError Statement
parseLuStat = P.parse statementP

parseLuFile :: String -> IO (Either P.ParseError Block)
parseLuFile = P.parseFromFile (const <$> blockP <*> P.eof)

{-
Unit Tests
---------

These unit tests summarize the tests given above.
-}

test_comb =
  "parsing combinators"
    ~: TestList
      [ P.parse (wsP P.alpha) "a" ~?= Right 'a',
        P.parse (many (wsP P.alpha)) "a b \n   \t c" ~?= Right "abc",
        P.parse (stringP "a") "a" ~?= Right (),
        P.parse (stringP "a") "b" ~?= Left "No parses",
        P.parse (many (stringP "a")) "a  a" ~?= Right [(), ()],
        P.parse (constP "&" 'a') "&  " ~?= Right 'a',
        P.parse (many (constP "&" 'a')) "&   &" ~?= Right "aa",
        P.parse (many (brackets (constP "1" 1))) "[1] [  1]   [1 ]" ~?= Right [1, 1, 1]
      ]

test_value =
  "parsing values"
    ~: TestList
      [ P.parse (many intValP) "1 2\n 3" ~?= Right [IntVal 1, IntVal 2, IntVal 3],
        P.parse (many boolValP) "true false\n true" ~?= Right [BoolVal True, BoolVal False, BoolVal True],
        P.parse (many nilValP) "nil nil\n nil" ~?= Right [NilVal, NilVal, NilVal],
        P.parse stringValP "\"a\"" ~?= Right (StringVal "a"),
        P.parse stringValP "\"a\\\"\"" ~?= Right (StringVal "a\\"),
        P.parse (many stringValP) "\"a\"   \"b\"" ~?= Right [StringVal "a", StringVal "b"],
        P.parse (many stringValP) "\" a\"   \"b\"" ~?= Right [StringVal " a", StringVal "b"]
      ]

test_exp =
  "parsing expressions"
    ~: TestList
      [ P.parse (many varP) "x y z" ~?= Right [Name "x", Name "y", Name "z"],
        P.parse varP "(x.y[1]).z" ~?= Right (Dot (Var (Proj (Var (Dot (Var (Name "x")) "y")) (Val (IntVal 1)))) "z"),
        P.parse (many nameP) "x sfds _ nil" ~?= Right ["x", "sfds", "_"],
        P.parse (many uopP) "- - #" ~?= Right [Neg, Neg, Len],
        P.parse (many bopP) "+ >= .." ~?= Right [Plus, Ge, Concat],
        P.parse tableConstP "{ x = 2, [3] = false }"
          ~?= Right (TableConst [FieldName "x" (Val (IntVal 2)), FieldKey (Val (IntVal 3)) (Val (BoolVal False))])
      ]

test_stat =
  "parsing statements"
    ~: TestList
      [ P.parse statementP ";" ~?= Right Empty,
        P.parse statementP "x=3" ~?= Right (Assign (Name "x") (Val (IntVal 3))),
        P.parse statementP "if x then y=nil else end"
          ~?= Right (If (Var (Name "x")) (Block [Assign (Name "y") (Val NilVal)]) (Block [])),
        P.parse statementP "while nil do end"
          ~?= Right (While (Val NilVal) (Block [])),
        P.parse statementP "repeat ; ; until false"
          ~?= Right (Repeat (Block [Empty, Empty]) (Val (BoolVal False)))
      ]

-- >>> runTestTT test_stat

{-
Testing summary
---------------
-}

test_all :: IO Counts
test_all = runTestTT $ TestList [test_comb, test_value, test_exp, test_stat]

-- >>> test_all                                    
-- Cases: 26  Tried: 26  Errors: 0  Failures: 0
-- Counts {cases = 26, tried = 26, errors = 0, failures = 0}
--

qc :: IO ()
qc = do
  putStrLn "roundtrip_val"
  QC.quickCheck prop_roundtrip_val
  putStrLn "roundtrip_exp"
  QC.quickCheck prop_roundtrip_exp
  putStrLn "roundtrip_stat"
  QC.quickCheck prop_roundtrip_stat
