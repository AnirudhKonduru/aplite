module UI (module UI) where

import Language ( aplOperators )
import qualified Parser as P
-- import qualified Stepper as S

import Brick
import Brick.Widgets.Border ( border )

import qualified Graphics.Vty as V
import qualified Brick.Widgets.Edit as E
import Brick.Widgets.Edit ( getEditContents )
import Control.Monad.Cont (MonadIO(liftIO))

newtype InputState = InputState { inputField :: E.Editor String () }


initialInputState :: InputState
initialInputState = InputState { inputField = E.editor () Nothing "" }

-- display messages to the UI
display :: String -> Widget ()
display msg = padLeft (Pad 1) $ str msg

-- Use the input field in a Brick app
app :: App InputState e ()
app = App { appDraw = drawUI
          , appChooseCursor = showFirstCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

-- convert string of operators into a list of operators (strings)
convert :: String -> [String]
convert = map (: [])

-- >>> convert "+-*"
-- ["+","-","*"]

-- Draw the UI
drawUI :: InputState -> [Widget ()]
drawUI st = [ui]
  where
    ui = border $ vBox [
                        padLeft (Pad 55) $ str "APLite - APL interpreter"
                       , padTop (Pad 1) $ E.renderEditor (str . unlines) True (inputField st)
                       , display "Press ESC/CTRL+C to exit | Press ENTER to evaluate"
                       , makeButtons $ convert aplOperators
                       ]

-- Handle events
handleEvent :: InputState -> BrickEvent () e -> EventM () (Next InputState)
handleEvent st (VtyEvent (V.EvKey V.KEsc [])) = halt st -- quit on escape
handleEvent st (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = halt st -- quit on ctrl+c

-- on enter, parse the input, interpret it, and print the result
handleEvent st (VtyEvent (V.EvKey V.KEnter [])) = do
  let contents = getEditContents $ inputField st

  -- ignore any empty input
  if contents == [""] then
    continue $ InputState { inputField = E.editor () Nothing "" }

  else do
    -- parse the input
    let parsed = P.parse P.expressionParser (unlines contents)

    -- TODO: interpret the input

    -- print the result
    case parsed of
      Right expr -> liftIO $ putStrLn $ "\n" ++ show expr ++ "\n"
      Left _ -> liftIO $ putStrLn $ "\n" ++ "Oops, something went wrong: " ++ "\n"
      -- clear editor
    continue $ st { inputField = E.editor () Nothing "" }

-- handle other events (regular input)
handleEvent st (VtyEvent e) = do
  st' <- E.handleEditorEvent e (inputField st)
  continue $ st { inputField = st' }
handleEvent st _ = continue st

-- Define the attribute map
theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (E.editAttr, V.white `on` V.black)
  , (E.editFocusedAttr, V.black `on` V.white)
  ]

-- clickable button
onClick :: Widget () -> Widget ()
onClick = clickable ()

-- a list of buttons (for APL's weird operators)
makeButtons :: [String] -> Widget ()
makeButtons [] = str ""
makeButtons (x:xs) = hBox [onClick $ str x, str " ", makeButtons xs]

-- Run the app
main :: IO ()
main = do
  defaultMain app initialInputState
  return ()
