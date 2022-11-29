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
import qualified Test.QuickCheck as QC

test_number :: Test
test_number =
  TestList
    [ 
      "number" ~: P.parse P.number "1" ~?= Right 1,
      "number" ~: P.parse P.number "-1" ~?= Right (-1),
      "positiveInt" ~: P.parse P.number "123" ~?= Right 123,
      "negativeInt" ~: P.parse P.number "-123" ~?= Right (-123),
      "zeroInt" ~: P.parse P.number "0" ~?= Right 0
    ]

test_float :: Test
test_float =
  TestList
    [ 
      "float" ~: P.parse P.float "1" ~?= Right 1.0,
      "positiveFloat" ~: P.parse P.float "123.456" ~?= Right 123.456,
      "negativeFloat" ~: P.parse P.float "-123.456" ~?= Right (-123.456),
      "zeroFloat" ~: P.parse P.float "0.0" ~?= Right 0.0
    ]

test_parser :: Test
test_parser =
  TestList
    [ 
      "test1" ~: P.parse P.scalarParser "1" ~?= Right (IntVal 1)
    ]