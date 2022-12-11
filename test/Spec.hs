import Language
import MkParser (parse')
import qualified MkParser as Parser
import Test.Hspec
import qualified BuiltInFunctions as Bif
import BuiltInOperators (reduce)

main :: IO ()
main = hspec $ do
  describe "number parser tests" $ do
    it "parses a number with -ve sign" $ do parse' Parser.number "¯123" `shouldBe` (-123)
    it "parses a number without sign" $ do parse' Parser.number "123" `shouldBe` 123
    it "parses a number with trailing decimal" $ do parse' Parser.number "123." `shouldBe` 123

  describe "float parser tests" $ do
    it "parses a float with -ve sign" $ do parse' Parser.float "¯123.456" `shouldBe` (-123.456)
    it "parses a float without sign" $ do parse' Parser.float "123.456" `shouldBe` 123.456
    it "parses a float with no whole part" $ do parse' Parser.float ".456" `shouldBe` 0.456
  -- it "parses a float with no fractional part" $ do parse' Parser.float "123." `shouldNotBe` 123.0

  describe "parse scalar value" $ do
    it "parses a scalar value" $ do parse' Parser.scalarParser "123" `shouldBe` (Number 123)

  describe "parse simple array values" $ do
    it "parses an array #1" $ do parse' Parser.arrayParser "1 2 3" `shouldBe` Array [3] [Scalar $ Number 1, Scalar $ Number 2, Scalar $ Number 3]
    it "parses an array #2" $ do parse' Parser.arrayParser "1 2 4.3 14" `shouldBe` Array [4] [Scalar $ Number 1, Scalar $ Number 2, Scalar $ Number 4.3, Scalar $ Number 14]

  describe "parse nested array values" $ do
    it "parses a nested array #1" $ do parse' Parser.arrayParser "1 2 (3 4)" `shouldBe` Array [3] [Scalar $ Number 1, Scalar $ Number 2, Array [2] [Scalar $ Number 3, Scalar $ Number 4]]
    it "parses a nested array #2" $ do parse' Parser.arrayParser "1 2 (3.1 (0.4 2)) 5" `shouldBe` Array [4] [Scalar $ Number 1, Scalar $ Number 2, Array [2] [Scalar $ Number 3.1, Array [2] [Scalar $ Number 0.4, Scalar $ Number 2]], Scalar $ Number 5]

  describe "parse series of values" $ do
    it "parses a series of values" $ do parse' Parser.valuesParser "1 2 3" `shouldBe` [Scalar (Number 1), Scalar (Number 2), Scalar (Number 3)]
    it "parse a series of values of different types" $ do parse' Parser.valuesParser "1 2 3 4.5" `shouldBe` [Scalar (Number 1), Scalar (Number 2), Scalar (Number 3), Scalar (Number 4.5)]

  describe "parse an expression" $ do
    it "parses a monadic expression" $ do parse' Parser.expressionParser "+ 10" `shouldBe` EMonadic Bif.conjugate (EArray [EValue (Scalar (Number 10.0))])
    it "parses a monadic expression #2" $ do parse' Parser.expressionParser "+ 1 2 3" `shouldBe` EMonadic Bif.conjugate (EArray [EValue (Scalar (Number 1.0)), EValue (Scalar (Number 2.0)), EValue (Scalar (Number 3.0))])
    it "parses a monadic expression with mixed types" $ do parse' Parser.expressionParser "+ 1 2 (3 4.5 (5 7 8) (- 5 2)) 9" `shouldBe` EMonadic Bif.conjugate (EArray [EValue (Scalar (Number 1)), EValue (Scalar (Number 2)), EArray [EValue (Scalar (Number 3)), EValue (Scalar (Number 4.5)), EArray [EValue (Scalar (Number 5)), EValue (Scalar (Number 7)), EValue (Scalar (Number 8))], EMonadic Bif.negate (EArray [EValue (Scalar (Number 5)), EValue (Scalar (Number 2))])], EValue (Scalar (Number 9))])
    it "parses a dyadic expression" $ do parse' Parser.expressionParser "1 + 2" `shouldBe` EDyadic Bif.plus (EArray [EValue $ Scalar (Number 1)]) (EArray [EValue (Scalar (Number 2))])
    it "parses a dyadic expression #2" $ do parse' Parser.expressionParser "1 5 6 + 2 3 8" `shouldBe` EDyadic Bif.plus (EArray [EValue (Scalar (Number 1.0)), EValue (Scalar (Number 5.0)), EValue (Scalar (Number 6.0))]) (EArray [EValue (Scalar (Number 2.0)), EValue (Scalar (Number 3.0)), EValue (Scalar (Number 8.0))])
    it "parses a dyadic expression with mixed types" $ do parse' Parser.expressionParser "2 3 (4 5.6 (7 8 9) (- 10 11)) 12 + 1 (- 10 11) (4 5)" `shouldBe` EDyadic Bif.plus (EArray [EValue (Scalar (Number 2.0)), EValue (Scalar (Number 3.0)), EArray [EValue (Scalar (Number 4.0)), EValue (Scalar (Number 5.6)), EArray [EValue (Scalar (Number 7.0)), EValue (Scalar (Number 8.0)), EValue (Scalar (Number 9.0))], EMonadic Bif.negate (EArray [EValue (Scalar (Number 10.0)), EValue (Scalar (Number 11.0))])], EValue (Scalar (Number 12.0))]) (EArray [EValue (Scalar (Number 1.0)), EMonadic Bif.negate (EArray [EValue (Scalar (Number 10.0)), EValue (Scalar (Number 11.0))]), EArray [EValue (Scalar (Number 4.0)), EValue (Scalar (Number 5.0))]])
    it "parses function prescedence corrctly" $ do parse' Parser.expressionParser "⍴ 2 3 ⍴ 3 4 5" `shouldBe` EMonadic Bif.shape (EDyadic Bif.reshape (EArray [EValue (Scalar (Number 2.0)),EValue (Scalar (Number 3.0))]) (EArray [EValue (Scalar (Number 3.0)),EValue (Scalar (Number 4.0)),EValue (Scalar (Number 5.0))]))

  describe "parse function expressions" $ do
    it "parses a monadic operator with dyadic function" $ do parse' Parser.expressionParser "+/ 1 2 3" `shouldBe` EMonadic (MOpDf (MonadicOperator "/" "reduce" reduce) Bif.plus) (EArray [EValue (Scalar (Number 1.0)), EValue (Scalar (Number 2.0)), EValue (Scalar (Number 3.0))])
    -- it "parses a monadic operator" $ do parse' Parser.operatorParser "-" `shouldBe` BuiltInMonadic "-" "negate" id