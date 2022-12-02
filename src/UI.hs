{-# OPTIONS_GHC -Wno-unused-imports #-}
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

-- import Brick
-- import Brick.Widgets.Core
-- import Brick.Types
-- import Brick.EventM
-- import Brick.Widgets.Edit
-- import Brick.Widgets.Border
-- import Brick.AttrMap
-- import Brick.Main


-- data AppState = AppState
--   { inputText :: String
--   }

-- initialState :: AppState
-- initialState = AppState
--   { inputText = ""
--   }


--   appEvent :: AppState -> BrickEvent n e -> EventM n (Next AppState)
-- appEvent st (VtyEvent e) =
--   case e of
--     EvKey KEnter [] -> halt st
--     EvKey KEsc [] -> halt st
--     EvKey (KChar c) [] -> continue $ st { inputText = inputText st ++ [c] }
--     _ -> continue st
-- appEvent st _ = continue st


-- -- | Define the main draw function of your application, which will render the current state to the terminal.

-- appDraw :: AppState -> [Widget n]
-- appDraw st = [border $ str (inputText st)]

-- main :: IO ()
-- main = defaultMain app initialState
--   where
--     app = App { appDraw = appDraw
--               , appHandleEvent = appEvent
--               , appStartEvent = return
--               , appAttrMap = const $ attrMap defAttr []
--               }
