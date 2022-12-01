module StepperSpec (module StepperSpec) where

import Test.HUnit
import qualified Test.HUnit as H
import Test.Hspec

main :: IO ()
main = hspec stepperSpec

stepperSpec :: Spec
stepperSpec = do
  describe "" $ do
    return ()