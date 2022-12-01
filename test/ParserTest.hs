{-# OPTIONS_GHC -Wno-unused-imports #-}
module ParserTest (module ParserTest) where
import Data.Void
import Language
import qualified Parser
import qualified Test.HUnit as H
import Test.Hspec
import Test.QuickCheck
import Text.Megaparsec
import Test.HUnit (Counts, Test (..), runTestTT, (~:), (~?=))

-- | basic tests for the every parser

test_number :: Test
test_number =
  "parsing numbers (ints)"
  ~: TestList
    [
      "number" ~: Parser.parse Parser.number "1" ~?= Right 1,
      "number" ~: Parser.parse Parser.number "-1" ~?= Right (-1),
      "positiveInt" ~: Parser.parse Parser.number "123" ~?= Right 123,
      "negativeInt" ~: Parser.parse Parser.number "-123" ~?= Right (-123),
      "zeroInt" ~: Parser.parse Parser.number "0" ~?= Right 0
    ]

test_float :: Test
test_float =
  "parsing floats"
  ~: TestList
    [
      "float" ~: Parser.parse Parser.float "1.0" ~?= Right 1.0,
      "float" ~: Parser.parse Parser.float "-1.0" ~?= Right (-1.0),
      "float" ~: Parser.parse Parser.float "0.567" ~?= Right 0.567,
      "float" ~: Parser.parse Parser.float "-0.567" ~?= Right (-0.567),
      "positiveFloat" ~: Parser.parse Parser.float "123.456" ~?= Right 123.456,
      "negativeFloat" ~: Parser.parse Parser.float "-123.456" ~?= Right (-123.456),
      "zeroFloat" ~: Parser.parse Parser.float "0.0" ~?= Right 0.0,
      "zeroFloat" ~: Parser.parse Parser.float "-0.0" ~?= Right 0.0
    ]

test_scalar :: Test
test_scalar =
  "parsing scalars"
  ~:
  TestList
    [
      "test" ~: Parser.parse Parser.scalarParser "1" ~?= Right (IntVal 1),
      "test" ~: Parser.parse Parser.scalarParser "123" ~?= Right (IntVal 123),
      "test" ~: Parser.parse Parser.scalarParser "-123" ~?= Right (IntVal (-123)),
      "test" ~: Parser.parse Parser.scalarParser "0" ~?= Right (IntVal 0),
      "test" ~: Parser.parse Parser.scalarParser "-0" ~?= Right (IntVal 0),
      "test" ~: Parser.parse Parser.scalarParser "-0.0" ~?= Right (FloatVal 0.0),
      "test" ~: Parser.parse Parser.scalarParser "0.0" ~?= Right (FloatVal 0.0),
      "test" ~: Parser.parse Parser.scalarParser "1.0" ~?= Right (FloatVal 1.0),
      "test" ~: Parser.parse Parser.scalarParser "12.0" ~?= Right (FloatVal 12.0),
      "test" ~: Parser.parse Parser.scalarParser "123.0" ~?= Right (FloatVal 123.0),
      "test" ~: Parser.parse Parser.scalarParser "-12.0" ~?= Right (FloatVal (-12.0)),
      "test" ~: Parser.parse Parser.scalarParser "1.23" ~?= Right (FloatVal 1.23)
    ]


test_value :: Test
test_value =
  "parsing value"
  ~:
  TestList
    [
      "test" ~: Parser.parse Parser.valueParser "1" ~?= Right (Scalar (IntVal 1)),
      "test" ~: Parser.parse Parser.valueParser "123" ~?= Right (Scalar (IntVal 123)),
      "test" ~: Parser.parse Parser.valueParser "-123" ~?= Right (Scalar (IntVal (-123))),
      "test" ~: Parser.parse Parser.valueParser "0" ~?= Right (Scalar (IntVal 0)),
      "test" ~: Parser.parse Parser.valueParser "-1.0" ~?= Right (Scalar (FloatVal (-1.0))),
      "test" ~: Parser.parse Parser.valueParser "0.0" ~?= Right (Scalar (FloatVal 0.0))
    ]

test_values :: Test
test_values =
  "parsing values"
  ~:
  TestList
    [
      "test" ~: Parser.parse Parser.valuesParser "1 2 3" ~?= Right [Scalar (IntVal 1),Scalar (IntVal 2),Scalar (IntVal 3)],
      "test" ~: Parser.parse Parser.valuesParser "1 2 3 4" ~?= Right [Scalar (IntVal 1),Scalar (IntVal 2),Scalar (IntVal 3),Scalar (IntVal 4)],
      "test" ~: Parser.parse Parser.valuesParser "1.0 1" ~?= Right [Scalar (FloatVal 1.0),Scalar (IntVal 1)],
      "test" ~: Parser.parse Parser.valuesParser "-1.1 3.14159265" ~?= Right [Scalar (FloatVal (-1.1)),Scalar (FloatVal 3.14159265)]
    ]


test_arrayOf :: Test
test_arrayOf =
  "testing array of"
  ~:
  TestList
    [
      "test" ~: Parser.arrayOf [Scalar (IntVal (-1))] ~?= Array [1] [Scalar (IntVal (-1))],
      "test" ~: Parser.arrayOf [Scalar (IntVal 1),Scalar (IntVal 2),Scalar (IntVal 3)] ~?= Array [3] [Scalar (IntVal 1),Scalar (IntVal 2),Scalar (IntVal 3)],
      "test" ~: Parser.arrayOf [] ~?= Array [0] []
    ]

test_array :: Test
test_array =
  "parsing arrays"
  ~:
  TestList
    [
      "test" ~: Parser.parse Parser.arrayParser "(1 2 3)" ~?= Right (Array [3] [Scalar (IntVal 1),Scalar (IntVal 2),Scalar (IntVal 3)]),
      "test" ~: Parser.parse Parser.arrayParser "(1 2 3 4)" ~?= Right (Array [4] [Scalar (IntVal 1),Scalar (IntVal 2),Scalar (IntVal 3),Scalar (IntVal 4)]),
      "test" ~: Parser.parse Parser.arrayParser "(1.0 1)" ~?= Right (Array [2] [Scalar (FloatVal 1.0),Scalar (IntVal 1)]),
      "test" ~: Parser.parse Parser.arrayParser "(-1.1 3.14159265)" ~?= Right (Array [2] [Scalar (FloatVal (-1.1)),Scalar (FloatVal 3.14159265)])
    ]

test_operator :: Test
test_operator =
  "parsing operators"
  ~:
  TestList
    [
      "test" ~: Parser.parse Parser.operator "+" ~?= Right '+',
      "test" ~: Parser.parse Parser.operator "-" ~?= Right '-',
      "test" ~: Parser.parse Parser.operator "*" ~?= Right '*',
      "test" ~: Parser.parse Parser.operator "⍝" ~?= Right '⍝',
      "test" ~: Parser.parse Parser.operator "⍵" ~?= Right '⍵'
    ]

test_monadic :: Test
test_monadic = 
  "parsing monadic operators"
  ~:
  TestList 
    [
      "test" ~: Parser.parse Parser.monadicParser "-1" ~?= Right (Monadic (MSym '-') (Value (Scalar (IntVal 1)))),
      "test" ~: Parser.parse Parser.monadicParser "∊ 1" ~?= Right (Monadic (MSym '∊') (Value (Scalar (IntVal 1))))
    ]

test_dyadic :: Test
test_dyadic = 
  "parsing dyadic operators"
  ~:
  TestList 
    [
      "test" ~: Parser.parse Parser.dyadicParser "1 + 2" ~?= Right (Dyadic (Value (Scalar (IntVal 1))) (DSym '+') (Value (Scalar (IntVal 2)))),
      "test" ~: Parser.parse Parser.dyadicParser "1 ≠ 2" ~?= Right (Dyadic (Value (Scalar (IntVal 1))) (DSym '≠') (Value (Scalar (IntVal 2))))
    ]



test_operand :: Test
test_operand =
  "parsing operands"
  ~:
  TestList
    [
      "test" ~: Parser.parse Parser.operandParser "1" ~?= Right (Value (Scalar (IntVal 1))),
      "test" ~: Parser.parse Parser.operandParser "(1 2 3)" ~?= Right (Value (Array [3] [Scalar (IntVal 1),Scalar (IntVal 2),Scalar (IntVal 3)])),
      "test" ~: Parser.parse Parser.operandParser "1.0 1" ~?= Right (Value (Array [2] [Scalar (FloatVal 1.0),Scalar (IntVal 1)])),
      "test" ~: Parser.parse Parser.operandParser "-1.1 3.14159265" ~?= Right (Value (Array [2] [Scalar (FloatVal (-1.1)),Scalar (FloatVal 3.14159265)]))
    ]

test_expression :: Test
test_expression = 
  "parsing expressions"
  ~:
  TestList 
    [
      "test" ~: Parser.parse Parser.expressionParser "1 2 3" ~?= Right (Value (Array [3] [Scalar (IntVal 1),Scalar (IntVal 2),Scalar (IntVal 3)])),
      "test" ~: Parser.parse Parser.expressionParser "1" ~?= Right (Value (Scalar (IntVal 1))),
      "test" ~: Parser.parse Parser.expressionParser "-10.21" ~?= Right (Monadic (MSym '-') (Value (Scalar (FloatVal 10.21)))),
      "test" ~: Parser.parse Parser.expressionParser "1 + 2" ~?= Right (Dyadic (Value (Scalar (IntVal 1))) (DSym '+') (Value (Scalar (IntVal 2)))),
      "test" ~: Parser.parse Parser.expressionParser "1 + 2 - 3" ~?= Right (Dyadic (Value (Scalar (IntVal 1))) (DSym '+') (Dyadic (Value (Scalar (IntVal 2))) (DSym '-') (Value (Scalar (IntVal 3))))),
      "test" ~: Parser.parse Parser.expressionParser "-1 + 2 - 2" ~?= Right (Monadic (MSym '-') (Dyadic (Value (Scalar (IntVal 1))) (DSym '+') (Dyadic (Value (Scalar (IntVal 2))) (DSym '-') (Value (Scalar (IntVal 2)))))),
      "test" ~: Parser.parse Parser.expressionParser "(1 2) + (2 3)" ~?= Right (Dyadic (Value (Array [2] [Scalar (IntVal 1),Scalar (IntVal 2)])) (DSym '+') (Value (Array [2] [Scalar (IntVal 2),Scalar (IntVal 3)])))
    ]

test_all_parsers :: IO Counts
test_all_parsers = runTestTT $ TestList [test_number, test_float, test_scalar, test_value, test_values, test_arrayOf, test_array, test_operator, test_monadic, test_dyadic, test_operand, test_expression]