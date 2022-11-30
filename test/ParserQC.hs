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

-- | quickcheck property for number parser
prop_number :: QC.Property
prop_number = QC.forAll QC.arbitrary $ \x -> P.parse P.number (show x) == Right x

-- | quickcheck property for float parser
prop_float :: QC.Property
prop_float = QC.forAll QC.arbitrary $ \x -> P.parse P.float (show x) == Right x

qc :: IO ()
qc = do
  putStrLn "prop_number"
  QC.quickCheck prop_number