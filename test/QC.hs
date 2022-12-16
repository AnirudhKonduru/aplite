module QC (module QC) where

import qualified MkParser as P
import qualified Eval as S
import qualified Language as L
import qualified BuiltInFunctions as Bif
import qualified BuiltInOperators as Bio

import qualified Test.QuickCheck as QC

import Data.Map (Map)
import qualified Data.Map as Map


import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)

-- | generate a random floar or random character
genScalar :: QC.Gen L.Scalar
genScalar = do
  number <- QC.arbitrary
  char <- QC.arbitrary
  QC.oneof [return $ L.Number number, return $ L.Char char]

instance QC.Arbitrary L.Scalar where
  arbitrary = genScalar

-- | generate a value
genValue :: Int -> QC.Gen L.Value
genValue 0 = do L.Scalar <$> genScalar
genValue n = do
  array <- QC.vectorOf n genScalar
  let array' = map L.Scalar array
  return $ L.Array [n] array'

instance QC.Arbitrary L.Value where
  arbitrary = QC.sized genValue

-- | Generate an EValue
genEValue :: QC.Gen L.Expression
genEValue = do L.EValue <$> QC.sized genValue

-- | Generate an EVariable
genEVariable :: QC.Gen L.Expression
genEVariable = do L.EVariable <$> QC.arbitrary

-- | Generate an EBind
genEBind :: QC.Gen L.Expression
genEBind = do
  var <- QC.arbitrary
  val <- QC.sized genValue
  return $ L.EBind var val

-- | Generate a function expression
-- genFunctionExpression :: QC.Gen L.FunctionExpression
-- genFunctionExpression = do
--   name <- QC.arbitrary

genMultidimArray :: QC.Gen L.Value
genMultidimArray = do
  n <- QC.choose (1, 10)
  m <- QC.choose (1, 10)
  array <- QC.vectorOf (n * m) genScalar
  let array' = map L.Scalar array
  return $ L.Array [n, m] array'




