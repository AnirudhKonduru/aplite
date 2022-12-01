{-# OPTIONS_GHC -Wno-unused-imports #-}
module StepperTest (module StepperTest) where
import Data.Void
import Language
import qualified Parser
import qualified Stepper
import Stepper (stepE, step, final, evalE, execute, RunTimeError (..))
import qualified Test.HUnit as H
import Test.Hspec
import Test.QuickCheck
import qualified Data.Map as Map
import Text.Megaparsec
import Test.HUnit (Counts, Test (..), runTestTT, (~:), (~?=))

stepTests :: Test
stepTests =
  "step" ~: TestList
    [ 
      "test" ~: step (Value (Scalar (IntVal 1))) Map.empty ~?= (Right (Value (Scalar (IntVal 1))), Map.empty),
      "test" ~: step (Variable "x") Map.empty ~?= (Left (UndefinedVariable "x"), Map.empty),
      "test" ~: step (Monadic (MSym '⍵') (Variable "x")) Map.empty ~?= (Left (UndefinedVariable "x"), Map.empty),
      "test" ~: step (Dyadic (Variable "x") (DSym '⍵') (Variable "x")) Map.empty ~?= (Left (UndefinedVariable "x"), Map.empty)
    ]