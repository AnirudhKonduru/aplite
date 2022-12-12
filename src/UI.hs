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

import UIHelper


data InputState = InputState { inputField :: E.Editor String (), keyboard :: Keyboard } 

instance Eq InputState where
  (InputState _ k1) == (InputState _ k2) = k1 == k2
  (InputState _ k1) /= (InputState _ k2) = k1 /= k2


initialInputState :: InputState
initialInputState = InputState { inputField = E.editor () Nothing "", keyboard = initKeyboard}

app :: App InputState e ()
app = App { appDraw = drawUI
          , appChooseCursor = showFirstCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

convert :: String -> [String]
convert = map (: [])

-- display messages to the UI
display :: String -> Widget ()
display msg = padLeft (Pad 1) $ str msg

drawUI :: InputState -> [Widget ()]
drawUI st = [ui]
  where
    ui = border $ vBox [
                        padLeft (Pad 55) $ str "APLite - APL interpreter"
                       , padTop (Pad 1) $ E.renderEditor (str . unlines) True (inputField st)
                       , padTop (Pad 0) $ drawGrid $ keyboard st
                       , drawInstructions "Press Ctrl + N for keyboard mapping | Press ESC/CTRL+C to exit | Press ENTER to evaluate"
                       ]

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


getKey :: V.Event -> Char
getKey (V.EvKey (V.KChar c) _) = c

handleEvent :: InputState -> BrickEvent () e -> EventM () (Next InputState)
handleEvent st (VtyEvent (V.EvKey V.KEsc [])) = halt st 
handleEvent st (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = halt st 

-- on clicking just control shift a, change the keyboard
handleEvent st (VtyEvent (V.EvKey (V.KChar 'n') [V.MCtrl])) = do
  let contents = getEditContents $ inputField st
  if keyboard st == initKeyboard then
    continue $ InputState { inputField = E.editor () Nothing (concat contents), keyboard = extraKeyboard }
  else
    continue $ InputState { inputField = E.editor () Nothing (concat contents), keyboard = initKeyboard }


-- on enter, parse the input, interpret it, and print the result
handleEvent st (VtyEvent (V.EvKey V.KEnter [])) = do
  let contents = getEditContents $ inputField st

  -- ignore any empty input
  if contents == [""] then
    continue $ InputState { inputField = E.editor () Nothing "", keyboard = initKeyboard }

  else do
    -- parse the input
    let parsed = P.parse P.expressionParser (unlines contents)

    -- TODO: interpret the parsed input

    -- print the result
    case parsed of
      Right expr -> liftIO $ putStrLn $ "\n" ++ show expr ++ "\n"
      Left _ -> liftIO $ putStrLn $ "\n" ++ "Oops, something went wrong: " ++ "\n"
    
    -- TODO: clear editor
    continue st

-- handle other events (regular input)
handleEvent st (VtyEvent e) = do
  st' <- E.handleEditorEvent e (inputField st)
  case keyboard st of
    initKeyboard -> continue $ st { inputField = st' }
    extraKeyboard -> do
      let contents = getEditContents $ inputField st
      -- get key from event
      let key = getKey e
      let newContents = concat contents ++ [mapKeyToGreek key]
      continue $ st { inputField = E.editor () Nothing newContents, keyboard = keyboard st}

handleEvent st _ = continue st


-- Run the app
main :: IO ()
main = do
  _ <- defaultMain app initialInputState
  return ()


