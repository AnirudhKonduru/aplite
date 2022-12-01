module UI (module UI) where

-- import Brick
-- import Brick.Widgets.Border

import Data.Void
import Language
  ( Dyadic (DSym),
    Expression (..),
    Monadic (MSym),
    Scalar (..),
    Value (..),
    aplOperators,
  )
import Text.Megaparsec
import Text.Megaparsec.Char (numberChar)
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Parser as P
import qualified Stepper as S

-- | input box
inputBox :: Widget
inputBox = undefined

-- | output box
outputBox :: Widget
outputBox = undefined

-- | border around the input box
inputBorder :: Widget
inputBorder = undefined

-- | border around the output box
outputBorder :: Widget
outputBorder = undefined

-- | main UI
ui :: Widget
ui = undefined




