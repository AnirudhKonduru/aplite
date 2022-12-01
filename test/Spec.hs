{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
import Data.Void
import Language
import qualified Parser
import qualified Test.HUnit as H
import Test.Hspec
import Test.QuickCheck
import Text.Megaparsec
import Test.HUnit (Counts, Test (..), runTestTT, (~:), (~?=))


makeTest :: (Eq a, Show a) => Parser.Parser a -> String -> a -> Test
makeTest p src expected = H.TestLabel src $ H.TestCase $ do
  let gote = parse p "" src
  case gote of
    Left e -> H.assertFailure $ show e
    Right got -> H.assertEqual src expected got

main :: IO ()
main = hspec $ do
  describe "parse a number" $ do
    it "parses a number with a +ve sign" $ do parse Parser.number "" "123" `shouldBe` Right 123
    it "parses a number with -ve sign" $ do parse Parser.number "" "-123" `shouldBe` Right (-123)
    it "parses zero" $ do parse Parser.number "" "0" `shouldBe` Right 0
    it "parses a number without sign" $ do parse Parser.number "" "+123" `shouldBe` Right 123
    it "parses a number with trailing decimal" $ do parse Parser.number "" "123." `shouldBe` Right 123

  describe "parse a float" $ do
    it "parses a float with a +ve sign" $ do parse Parser.float "" "123.456" `shouldBe` Right 123.456
    it "parses a float with -ve sign" $ do parse Parser.float "" "-123.456" `shouldBe` Right (-123.456)
    it "parses a float without sign" $ do parse Parser.float "" "+123.456" `shouldBe` Right 123.456
    it "parses a float with no whole part" $ do parse Parser.float "" ".456" `shouldBe` Right 0.456
    it "parses a float with no fractional part" $ do parse Parser.float "" "123." `shouldNotBe` Right 123.0

  describe "parse scalar value" $ do
    it "parses a scalar value" $ do parse Parser.scalarParser "" "123" `shouldBe` Right (IntVal 123)

  describe "parse simple array values" $ do
    it "parses an array #1" $ do parse Parser.arrayParser "" "1 2 3" `shouldBe` Right (Array [3] [Scalar $ IntVal 1, Scalar $ IntVal 2, Scalar $ IntVal 3])
    it "parses an array #2" $ do parse Parser.arrayParser "" "1 0.2 4.3 14" `shouldBe` Right (Array [4] [Scalar $ IntVal 1, Scalar $ FloatVal 0.2, Scalar $ FloatVal 4.3, Scalar $ IntVal 14])

  describe "parse nested array values" $ do
    it "parses a nested array #1" $ do parse Parser.arrayParser "" "1 2 (3 4)" `shouldBe` Right (Array [3] [Scalar $ IntVal 1, Scalar $ IntVal 2, Array [2] [Scalar $ IntVal 3, Scalar $ IntVal 4]])
    it "parses a nested array #2" $ do parse Parser.arrayParser "" "1 2 (3.1 (0.4 2)) 5" `shouldBe` Right (Array [4] [Scalar $ IntVal 1, Scalar $ IntVal 2, Array [2] [Scalar $ FloatVal 3.1, Array [2] [Scalar $ FloatVal 0.4, Scalar $ IntVal 2]], Scalar $ IntVal 5])

  describe "parse a single value" $ do
    it "parses a scalar value" $ do parse Parser.valueParser "" "123" `shouldBe` Right (Scalar $ IntVal 123)
    it "parses an array value" $ do parse Parser.valueParser "" "1 2 3" `shouldBe` Right (Array [3] [Scalar $ IntVal 1, Scalar $ IntVal 2, Scalar $ IntVal 3])

  describe "parse series of values" $ do
    it "parses a series of values" $ do parse Parser.valuesParser "" "1 2 3" `shouldBe` Right [Scalar (IntVal 1), Scalar (IntVal 2), Scalar (IntVal 3)]
    it "parse a series of values of different types" $ do parse Parser.valuesParser "" "1 2 3 4.5" `shouldBe` Right [Scalar (IntVal 1), Scalar (IntVal 2), Scalar (IntVal 3), Scalar (FloatVal 4.5)]


  describe "parse an expression" $ do
    it "parses a monadic expression" $ do parse Parser.expressionParser "" "+ 10" `shouldBe` Right (Monadic (MSym '+') (Value (Scalar (IntVal 10))))
    it "parses a monadic expression #2" $ do parse Parser.expressionParser "" "+ (1 2 3)" `shouldBe` Right (Monadic (MSym '+') (Value (Array [3] [Scalar (IntVal 1), Scalar (IntVal 2), Scalar (IntVal 3)])))
    it "parses a monadic expression with mixed types" $ do parse Parser.expressionParser "" "+ 1 2 (3 4.5 (5 7 8) (- 5 2)) 9" `shouldBe` Right (Monadic (MSym '+') (Value (Array [4] [Scalar (IntVal 1), Scalar (IntVal 2), Array [4] [Scalar (IntVal 3), Scalar (FloatVal 4.5), Array [3] [Scalar (IntVal 5), Scalar (IntVal 7), Scalar (IntVal 8)], Expression (Monadic (MSym '-') (Value (Array [2] [Scalar (IntVal 5), Scalar (IntVal 2)])))], Scalar (IntVal 9)])))
    it "parses a dyadic expression" $ do parse Parser.expressionParser "" "1 + 2" `shouldBe` Right (Dyadic (Value (Scalar (IntVal 1))) (DSym '+') (Value (Scalar (IntVal 2))))
    it "parses a dyadic expression #2" $ do parse Parser.expressionParser "" "1 5 6 + 2 3 8" `shouldBe` Right (Dyadic (Value (Array [3] [Scalar (IntVal 1), Scalar (IntVal 5), Scalar (IntVal 6)])) (DSym '+') (Value (Array [3] [Scalar (IntVal 2), Scalar (IntVal 3), Scalar (IntVal 8)])))
    it "parses a dyadic expression with mixed types" $ do parse Parser.expressionParser "" "2 3 (4 5.6 (7 8 9) (- 10 11)) 12 + 1 (- 10 11) (4 5)" `shouldBe` Right (Dyadic (Value (Array [4] [Scalar (IntVal 2), Scalar (IntVal 3), Array [4] [Scalar (IntVal 4), Scalar (FloatVal 5.6), Array [3] [Scalar (IntVal 7), Scalar (IntVal 8), Scalar (IntVal 9)], Expression (Monadic (MSym '-') (Value (Array [2] [Scalar (IntVal 10), Scalar (IntVal 11)])))], Scalar (IntVal 12)])) (DSym '+') (Value (Array [3] [Scalar (IntVal 1), Expression (Monadic (MSym '-') (Value (Array [2] [Scalar (IntVal 10), Scalar (IntVal 11)]))), Array [2] [Scalar (IntVal 4), Scalar (IntVal 5)]])))