{-# LANGUAGE ScopedTypeVariables #-}

module ParserTests where

import LuSyntax
import LuParser
import Formatter
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import qualified Test.QuickCheck as QC
import qualified Parser as P
import Control.Applicative

test_wsP :: Test
test_wsP =
  TestList
    [ P.parse (wsP P.alpha) "a" ~?= Right 'a',
      P.parse (many (wsP P.alpha)) "a b \n   \t c" ~?= Right "abc"
    ]


test_stringP :: Test
test_stringP =
  TestList
    [ P.parse (stringP "a") "a" ~?= Right (),
      P.parse (stringP "a") "b" ~?= Left "No parses",
      P.parse (many (stringP "a")) "a  a" ~?= Right [(), ()]
    ]

test_constP :: Test
test_constP =
  TestList
    [ P.parse (constP "&" 'a') "&  " ~?= Right 'a',
      P.parse (many (constP "&" 'a')) "&   &" ~?= Right "aa"
    ]


test_stringValP :: Test
test_stringValP =
  TestList
    [ P.parse stringValP "\"a\"" ~?= Right (StringVal "a"),
      P.parse stringValP "\"a\\\"\"" ~?= Right (StringVal "a\\"),
      P.parse (many stringValP) "\"a\"   \"b\"" ~?= Right [StringVal "a", StringVal "b"],
      P.parse (many stringValP) "\" a\"   \"b\"" ~?= Right [StringVal " a", StringVal "b"]
    ]


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



test_all :: IO Counts
test_all = runTestTT $ TestList [test_comb, test_value, test_exp, test_stat]

-- | parsing and unparsing should not affect the block 
prop_sameMeaning :: Block -> Bool
prop_sameMeaning b = parseFromText (formatBlock b) == Right b


prop_roundtrip_val :: Value -> Bool
prop_roundtrip_val v = P.parse valueP (pretty v) == Right v

prop_roundtrip_exp :: Expression -> Bool
prop_roundtrip_exp e = P.parse expP (pretty e) == Right e

prop_roundtrip_stat :: Statement -> Bool
prop_roundtrip_stat s = P.parse statementP (pretty s) == Right s

qc :: IO ()
qc = do
  putStrLn "roundtrip_val"
  QC.quickCheck prop_roundtrip_val
  putStrLn "roundtrip_exp"
  QC.quickCheck prop_roundtrip_exp
  putStrLn "roundtrip_stat"
  QC.quickCheck prop_roundtrip_stat
  putStrLn "same_meaning"
  QC.quickCheck prop_sameMeaning

runParserTests :: IO ()
runParserTests = do
    test_all
    qc


