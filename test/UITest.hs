module UITest (module UITest) where

import Language
import qualified Parser as P
import qualified Stepper as S
import qualified UI as U

import Stepper (stepE, step, final, evalE, execute, RunTimeError (..))
import qualified Test.HUnit as H
import Test.Hspec
import Test.QuickCheck
import qualified Data.Map as Map
import Text.Megaparsec
import Test.HUnit (Counts, Test (..), runTestTT, (~:), (~?=))


test_convert :: Test
test_convert = 
  "test_convert" 
  ~: TestList [
    "test" ~: U.convert "+-*" ~?= ["+", "-", "*"]
  ]
