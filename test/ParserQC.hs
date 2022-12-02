{-# OPTIONS_GHC -Wno-unused-imports #-}
module ParserQC (module ParserQC) where

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
import Test.QuickCheck (Arbitrary)


genScalar :: QC.Gen Scalar
genScalar = do
  int <- QC.arbitrary
  float <- QC.arbitrary
  QC.oneof [return $ IntVal int, return $ FloatVal float]

genValue :: QC.Gen Value
genValue = do
  scalar <- genScalar
  int <- QC.arbitrary
  value <- genValue
  QC.oneof [return $ Scalar scalar, return $ Array [int] [value]]

genExpression :: QC.Gen Expression
genExpression = do
  value <- genValue
  monadic <- genMonadic
  dyadic <- genDyadic
  expression <- genExpression
  QC.oneof [return $ Value value, return $ Monadic monadic expression, return $ Dyadic expression dyadic expression]

genMonadic :: QC.Gen Monadic
genMonadic = do
  char <- QC.elements aplOperators
  return $ MSym char

genDyadic :: QC.Gen Dyadic
genDyadic = do
  char <- QC.elements aplOperators
  return $ DSym char

instance Arbitrary Scalar where
  arbitrary = genScalar
  shrink = const []

instance Arbitrary Value where
  arbitrary = genValue
  shrink = const []


instance Arbitrary Expression where
  arbitrary = genExpression
  shrink = const []

instance Arbitrary Monadic where
  arbitrary = genMonadic
  shrink = const []


instance Arbitrary Dyadic where
  arbitrary = genDyadic
  shrink = const []


prop_number :: Int -> Bool
prop_number n =  P.parse P.number (show n) == Right n

prop_float :: Float -> Bool
prop_float f = P.parse P.float (show f) == Right f

prop_expression :: Expression -> Bool
prop_expression e = P.parse P.expressionParser (show e) == Right e

prop_monadic :: Expression -> Bool
prop_monadic m = P.parse P.monadicParser (show m) == Right m


qc :: IO ()
qc = do
  putStrLn "Running quickcheck tests"
  putStr "Number parser: "
  QC.quickCheck prop_number
  putStr "Float parser: "
  QC.quickCheck prop_float
