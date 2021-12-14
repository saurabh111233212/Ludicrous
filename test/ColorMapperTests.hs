module ColorMapperTests where

import ColorMapper
import qualified Data.Functor.Identity
import Data.Text (pack)
import Test.HUnit
import Text.Parsec
import Text.Parsec.Error

-- | Helper function that runs a parser from scratch
run ::
  Stream s Data.Functor.Identity.Identity t =>
  Parsec s () a ->
  s ->
  Either String a
run parser s = case runParser parser () "" s of
  Left _ -> Left "error"
  Right x -> Right x

-- tests the whitespace parser
testWhiteSpaceParser :: Test
testWhiteSpaceParser =
  "Whitespace Parsing Test"
    ~: TestList
      [ run wsParser " " ~?= Right (pack " ", C {attribute = "whitespace"}),
        run wsParser "\t" ~?= Right (pack "\t", C {attribute = "whitespace"}),
        run wsParser " some other stuff"
          ~?= Right (pack " ", C {attribute = "whitespace"}),
        run wsParser "some other stuff " ~?= Left "error",
        run wsParser "    \t    "
          ~?= Right (pack "    \t    ", C {attribute = "whitespace"}),
        run wsParser "" ~?= Left "error"
      ]

-- test string parser
testStringParser :: Test
testStringParser =
  "String Parsing Test"
    ~: TestList
      [ run stringParser "\"\""
          ~?= Right (pack "\"\"", C {attribute = "string"}),
        run stringParser "\" something \""
          ~?= Right (pack "\" something \"", C {attribute = "string"}),
        run stringParser "some other stuff" ~?= Left "error",
        run stringParser "\"if else then +\""
          ~?= Right (pack "\"if else then +\"", C {attribute = "string"}),
        run stringParser "" ~?= Left "error",
        run stringParser "\" incomplete string" ~?= Left "error"
      ]

-- test string parser
testCharParser :: Test
testCharParser =
  "Character Parsing Test"
    ~: TestList
      [ run charParser "\'x\'"
          ~?= Right (pack "\'x\'", C {attribute = "string"}),
        run charParser "\' something \'" ~?= Left "error",
        run charParser "some other stuff" ~?= Left "error",
        run charParser "\'\'" ~?= Left "error",
        run charParser "" ~?= Left "error",
        run charParser "\' incomplete string" ~?= Left "error"
      ]

-- test string parser
testKeywordParser :: Test
testKeywordParser =
  "Keyword Parsing Test"
    ~: TestList
      [ run keyParser "\"if\"" ~?= Left "error",
        run keyParser "i" ~?= Left "error",
        run keyParser "" ~?= Left "error",
        run keyParser "if" ~?= Right (pack "if", C {attribute = "keyword"}),
        run keyParser "ifword" ~?= Left "error",
        run keyParser "if other"
          ~?= Right (pack "if", C {attribute = "keyword"})
      ]

-- test string parser
testOperatorParser :: Test
testOperatorParser =
  "Operator Parsing Test"
    ~: TestList
      [ run opParser "" ~?= Left "error",
        run opParser "something else" ~?= Left "error",
        run opParser "+" ~?= Right (pack "+", C {attribute = "operator"}),
        run opParser "+1" ~?= Right (pack "+", C {attribute = "operator"}),
        run opParser "+ " ~?= Right (pack "+", C {attribute = "operator"}),
        run opParser "==" ~?= Right (pack "==", C {attribute = "operator"})
      ]

-- test misc parser
testMiscParser :: Test
testMiscParser =
  "Misc Parsing Test"
    ~: TestList
      [ run miscParser "+" ~?= Left "error",
        run miscParser "" ~?= Left "error",
        run miscParser "ifsomething"
          ~?= Right (pack "ifsomething", C {attribute = "text"}),
        run miscParser "ifsome\nthing"
          ~?= Right (pack "ifsome", C {attribute = "text"})
      ]

-- tests if correctly parses some lines
testColorMapper :: Test
testColorMapper =
  "Color Mapper Test"
    ~: TestList
      [ colorMap (pack "") ~?= [(pack " ", C {attribute = "text"})],
        colorMap
          (pack "if x == 2 then y else z repeat while end \"test string\" too")
          ~?= [ (pack "if", C {attribute = "keyword"}),
                (pack " ", C {attribute = "whitespace"}),
                (pack "x", C {attribute = "text"}),
                (pack " ", C {attribute = "whitespace"}),
                (pack "==", C {attribute = "operator"}),
                (pack " ", C {attribute = "whitespace"}),
                (pack "2", C {attribute = "text"}),
                (pack " ", C {attribute = "whitespace"}),
                (pack "then", C {attribute = "keyword"}),
                (pack " ", C {attribute = "whitespace"}),
                (pack "y", C {attribute = "text"}),
                (pack " ", C {attribute = "whitespace"}),
                (pack "else", C {attribute = "keyword"}),
                (pack " ", C {attribute = "whitespace"}),
                (pack "z", C {attribute = "text"}),
                (pack " ", C {attribute = "whitespace"}),
                (pack "repeat", C {attribute = "keyword"}),
                (pack " ", C {attribute = "whitespace"}),
                (pack "while", C {attribute = "keyword"}),
                (pack " ", C {attribute = "whitespace"}),
                (pack "end", C {attribute = "keyword"}),
                (pack " ", C {attribute = "whitespace"}),
                (pack "\"test string\"", C {attribute = "string"}),
                (pack " ", C {attribute = "whitespace"}),
                (pack "too", C {attribute = "text"})
              ]
      ]

-- runs all of the color mapper tests
runColorMapperTests :: IO ()
runColorMapperTests =
  putStrLn "Running Color Mapper tests"
    >> putStrLn "Whitespace parser tests"
    >> runTestTT testWhiteSpaceParser
    >> putStrLn "String parser tests"
    >> runTestTT testStringParser
    >> putStrLn "Character parser tests"
    >> runTestTT testCharParser
    >> putStrLn "Keyword parser tests"
    >> runTestTT testKeywordParser
    >> putStrLn "Operation parser tests"
    >> runTestTT testOperatorParser
    >> putStrLn "Misc parser tests"
    >> runTestTT testMiscParser
    >> runTestTT testColorMapper
    >> putStrLn "Color Mapper Tests Complete"