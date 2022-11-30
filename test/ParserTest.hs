module ParserTest (module ParserTest) where

import qualified Parser as P
import Language
  ( Dyadic (DSym),
    Expression (..),
    Monadic (MSym),
    Scalar (..),
    Value (..),
    aplOperators,
  )
import Test.HUnit (Counts, Test (..), runTestTT, (~:), (~?=))


-- | basic tests for the every parser

test_number :: Test
test_number =
  "parsing numbers (ints)"
  ~: TestList
    [
      "number" ~: P.parse P.number "1" ~?= Right 1,
      "number" ~: P.parse P.number "-1" ~?= Right (-1),
      "positiveInt" ~: P.parse P.number "123" ~?= Right 123,
      "negativeInt" ~: P.parse P.number "-123" ~?= Right (-123),
      "zeroInt" ~: P.parse P.number "0" ~?= Right 0
    ]

test_float :: Test
test_float =
  "parsing floats"
  ~: TestList
    [
      "float" ~: P.parse P.float "1.0" ~?= Right 1.0,
      "float" ~: P.parse P.float "-1.0" ~?= Right (-1.0),
      "float" ~: P.parse P.float "0.567" ~?= Right 0.567,
      "float" ~: P.parse P.float "-0.567" ~?= Right (-0.567),
      "positiveFloat" ~: P.parse P.float "123.456" ~?= Right 123.456,
      "negativeFloat" ~: P.parse P.float "-123.456" ~?= Right (-123.456),
      "zeroFloat" ~: P.parse P.float "0.0" ~?= Right 0.0,
      "zeroFloat" ~: P.parse P.float "-0.0" ~?= Right 0.0
    ]

test_scalar :: Test
test_scalar =
  "parsing scalars"
  ~:
  TestList
    [
      "test" ~: P.parse P.scalarParser "1" ~?= Right (IntVal 1),
      "test" ~: P.parse P.scalarParser "123" ~?= Right (IntVal 123),
      "test" ~: P.parse P.scalarParser "-123" ~?= Right (IntVal (-123)),
      "test" ~: P.parse P.scalarParser "0" ~?= Right (IntVal 0),
      "test" ~: P.parse P.scalarParser "-0" ~?= Right (IntVal 0),
      "test" ~: P.parse P.scalarParser "-0.0" ~?= Right (FloatVal 0.0),
      "test" ~: P.parse P.scalarParser "0.0" ~?= Right (FloatVal 0.0),
      "test" ~: P.parse P.scalarParser "1.0" ~?= Right (FloatVal 1.0),
      "test" ~: P.parse P.scalarParser "12.0" ~?= Right (FloatVal 12.0),
      "test" ~: P.parse P.scalarParser "123.0" ~?= Right (FloatVal 123.0),
      "test" ~: P.parse P.scalarParser "-12.0" ~?= Right (FloatVal (-12.0)),
      "test" ~: P.parse P.scalarParser "1.23" ~?= Right (FloatVal 1.23)
    ]


test_value :: Test
test_value =
  "parsing value"
  ~:
  TestList
    [
      "test" ~: P.parse P.valueParser "1" ~?= Right (Scalar (IntVal 1)),
      "test" ~: P.parse P.valueParser "123" ~?= Right (Scalar (IntVal 123)),
      "test" ~: P.parse P.valueParser "-123" ~?= Right (Scalar (IntVal (-123))),
      "test" ~: P.parse P.valueParser "0" ~?= Right (Scalar (IntVal 0)),
      "test" ~: P.parse P.valueParser "-1.0" ~?= Right (Scalar (FloatVal (-1.0))),
      "test" ~: P.parse P.valueParser "0.0" ~?= Right (Scalar (FloatVal 0.0))
    ]

test_values :: Test
test_values =
  "parsing values"
  ~:
  TestList
    [
      "test" ~: P.parse P.valuesParser "1 2 3" ~?= Right [Scalar (IntVal 1),Scalar (IntVal 2),Scalar (IntVal 3)],
      "test" ~: P.parse P.valuesParser "1 2 3 4" ~?= Right [Scalar (IntVal 1),Scalar (IntVal 2),Scalar (IntVal 3),Scalar (IntVal 4)],
      "test" ~: P.parse P.valuesParser "1.0 1" ~?= Right [Scalar (FloatVal 1.0),Scalar (IntVal 1)],
      "test" ~: P.parse P.valuesParser "-1.1 3.14159265" ~?= Right [Scalar (FloatVal (-1.1)),Scalar (FloatVal 3.14159265)]
    ]


test_arrayOf :: Test
test_arrayOf =
  "testing array of"
  ~:
  TestList
    [
      "test" ~: P.arrayOf [Scalar (IntVal (-1))] ~?= Array [1] [Scalar (IntVal (-1))],
      "test" ~: P.arrayOf [Scalar (IntVal 1),Scalar (IntVal 2),Scalar (IntVal 3)] ~?= Array [3] [Scalar (IntVal 1),Scalar (IntVal 2),Scalar (IntVal 3)],
      "test" ~: P.arrayOf [] ~?= Array [0] []
    ]

test_array :: Test
test_array =
  "parsing arrays"
  ~:
  TestList
    [
      "test" ~: P.parse P.arrayParser "(1 2 3)" ~?= Right (Array [3] [Scalar (IntVal 1),Scalar (IntVal 2),Scalar (IntVal 3)]),
      "test" ~: P.parse P.arrayParser "(1 2 3 4)" ~?= Right (Array [4] [Scalar (IntVal 1),Scalar (IntVal 2),Scalar (IntVal 3),Scalar (IntVal 4)]),
      "test" ~: P.parse P.arrayParser "(1.0 1)" ~?= Right (Array [2] [Scalar (FloatVal 1.0),Scalar (IntVal 1)]),
      "test" ~: P.parse P.arrayParser "(-1.1 3.14159265)" ~?= Right (Array [2] [Scalar (FloatVal (-1.1)),Scalar (FloatVal 3.14159265)])
    ]

test_operator :: Test
test_operator =
  "parsing operators"
  ~:
  TestList
    [
      "test" ~: P.parse P.operator "+" ~?= Right '+',
      "test" ~: P.parse P.operator "-" ~?= Right '-',
      "test" ~: P.parse P.operator "*" ~?= Right '*',
      "test" ~: P.parse P.operator "⍝" ~?= Right '⍝',
      "test" ~: P.parse P.operator "⍵" ~?= Right '⍵'
    ]

test_monadic :: Test
test_monadic = 
  "parsing monadic operators"
  ~:
  TestList 
    [
      "test" ~: P.parse P.monadicParser "-1" ~?= Right (Monadic (MSym '-') (Value (Scalar (IntVal 1)))),
      "test" ~: P.parse P.monadicParser "∊ 1" ~?= Right (Monadic (MSym '∊') (Value (Scalar (IntVal 1))))
    ]

test_dyadic :: Test
test_dyadic = 
  "parsing dyadic operators"
  ~:
  TestList 
    [
      "test" ~: P.parse P.dyadicParser "1 + 2" ~?= Right (Dyadic (Value (Scalar (IntVal 1))) (DSym '+') (Value (Scalar (IntVal 2)))),
      "test" ~: P.parse P.dyadicParser "1 ≠ 2" ~?= Right (Dyadic (Value (Scalar (IntVal 1))) (DSym '≠') (Value (Scalar (IntVal 2))))
    ]



test_operand :: Test
test_operand =
  "parsing operands"
  ~:
  TestList
    [
      "test" ~: P.parse P.operandParser "1" ~?= Right (Value (Scalar (IntVal 1))),
      "test" ~: P.parse P.operandParser "1 2 3" ~?= Right (Value (Array [3] [Scalar (IntVal 1),Scalar (IntVal 2),Scalar (IntVal 3)])),
      "test" ~: P.parse P.operandParser "1 2 3 4" ~?= Right (Value (Array [4] [Scalar (IntVal 1),Scalar (IntVal 2),Scalar (IntVal 3),Scalar (IntVal 4)])),
      "test" ~: P.parse P.operandParser "1.0 1" ~?= Right (Value (Array [2] [Scalar (FloatVal 1.0),Scalar (IntVal 1)])),
      "test" ~: P.parse P.operandParser "-1.1 3.14159265" ~?= Right (Value (Array [2] [Scalar (FloatVal (-1.1)),Scalar (FloatVal 3.14159265)])),
      "test" ~: P.parse P.operandParser "1 + 2" ~?= Right (Dyadic (Value (Scalar (IntVal 1))) (DSym '+') (Value (Scalar (IntVal 2))))
    ]

test_expression :: Test
test_expression = 
  "parsing expressions"
  ~:
  TestList 
    [
      "test" ~: P.parse P.expressionParser "1 2 3" ~?= Right (Value (Array [3] [Scalar (IntVal 1),Scalar (IntVal 2),Scalar (IntVal 3)])),
      "test" ~: P.parse P.expressionParser "1" ~?= Right (Value (Scalar (IntVal 1))),
      "test" ~: P.parse P.expressionParser "-10.21" ~?= Right (Monadic (MSym '-') (Value (Scalar (FloatVal 10.21)))),
      "test" ~: P.parse P.expressionParser "1 + 2" ~?= Right (Dyadic (Value (Scalar (IntVal 1))) (DSym '+') (Value (Scalar (IntVal 2)))),
      "test" ~: P.parse P.expressionParser "1 + 2 - 3" ~?= Right (Dyadic (Value (Scalar (IntVal 1))) (DSym '+') (Dyadic (Value (Scalar (IntVal 2))) (DSym '-') (Value (Scalar (IntVal 3))))),
      "test" ~: P.parse P.expressionParser "-1 + 2 - 2" ~?= Right (Monadic (MSym '-') (Dyadic (Value (Scalar (IntVal 1))) (DSym '+') (Dyadic (Value (Scalar (IntVal 2))) (DSym '-') (Value (Scalar (IntVal 2))))))
    ]

test_all_parsers :: IO Counts
test_all_parsers = runTestTT $ TestList [test_number, test_float, test_scalar, test_value, test_values, test_arrayOf, test_array, test_operator]