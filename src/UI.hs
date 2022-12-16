module UI (module UI) where

import Language
import qualified MkParser as M
import qualified Eval as S

import Brick
import Brick.Widgets.Border ( border )

import qualified Graphics.Vty as V
import qualified Brick.Widgets.Edit as E
import Brick.Widgets.Edit ( getEditContents )
import Control.Monad.Cont (MonadIO(liftIO))

import KeyMapping

-- | State passed to the Brick App: Has an input field and a keyboard (since we have the option to switch between keyboards)
data InputState = InputState { inputField :: E.Editor String Name, keyboard :: Keyboard } 

-- | Eq instance for input states
instance Eq InputState where
  (InputState _ k1) == (InputState _ k2) = k1 == k2
  (InputState _ k1) /= (InputState _ k2) = k1 /= k2

-- | Initial input state when the app starts
initialInputState :: InputState
initialInputState = InputState { inputField = E.editor Edit Nothing "", keyboard = initKeyboard}

-- | The main brick app (takes in UI, State, Event, and eventHandler)
app :: App InputState e Name
app = App { appDraw = drawUI
          -- or sctroll ro end of input
          , appChooseCursor = showFirstCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

convert :: String -> [String]
convert = map (: [])

-- display messages to the UI 
display :: String -> Widget Name
display msg = padLeft (Pad 1) $ str msg

-- | Draw the actual UI: write the title, render the input field, render the keyboard, and display any instructions
drawUI :: InputState -> [Widget Name]
drawUI st = [ui]
  where
    ui = border $ vBox [
                        padLeft (Pad 55) $ str "APLite - APL interpreter"
                       , padTop (Pad 1) $ E.renderEditor (str . unlines) True (inputField st)
                       , padTop (Pad 0) $ drawGrid $ keyboard st
                       , drawInstructions "Press Ctrl + N for keyboard mapping | Ctrl + C to clear | Press ESC/CTRL+C to exit | Press ENTER to evaluate"
                       ]

-- Define the attribute map (for UI elements like color, etc.)
theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (E.editAttr, V.white `on` V.black)
  , (E.editFocusedAttr, V.black `on` V.white)
  ]

-- | when a key is pressed, get the character from the Key event
getKey :: V.Event -> Char
getKey (V.EvKey (V.KChar c) _) = c

-- | Event handler function (Key presses, mouse clicks, etc.)
handleEvent :: InputState -> BrickEvent Name e -> EventM Name (Next InputState)

-- on clicking ESC, exit the app
handleEvent st (VtyEvent (V.EvKey V.KEsc [])) = halt st 

-- on clicking Ctrl N, switch between keyboards
handleEvent st (VtyEvent (V.EvKey (V.KChar 'n') [V.MCtrl])) = do
  let contents = getEditContents $ inputField st
  if keyboard st == initKeyboard then
    continue $ InputState { inputField = E.editor EditLines Nothing (concat contents), keyboard = aplKeyboard }
  else
    continue $ InputState { inputField = E.editor EditLines Nothing (concat contents), keyboard = initKeyboard }

-- on clicking Ctrl+c, clear the input field
handleEvent st (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = do
  continue $ InputState { inputField = E.editor EditLines Nothing "", keyboard = keyboard st }

-- On clicking Ctrl + key, add the corresponding APL character to the input
handleEvent st (VtyEvent (V.EvKey (V.KChar key) [V.MCtrl])) = do
  let contents = getEditContents $ inputField st
  let newContents = concat contents ++ [mapKeyToApl key]
  continue $ st { inputField = E.editor EditLines Nothing newContents, keyboard = keyboard st}

-- On clicking enter, parse the input, interpret it, and print the result
handleEvent st (VtyEvent (V.EvKey V.KEnter [])) = do
  let contents = getEditContents $ inputField st

  -- ignore any empty input
  if contents == [""] then
    continue $ InputState { inputField = E.editor EditLines Nothing "", keyboard = initKeyboard }

  -- parse the input
  else do
    let result = S.run $ concat contents
    let newContents = concat contents ++ "\n--> \n" ++ result ++ "\n"
    continue st { inputField = E.editor EditLines Nothing newContents, keyboard = initKeyboard }

-- On clicking any other key (regular keys like alphabet, numbers, etc.) simply add the key to the input
handleEvent st (VtyEvent e) = do
  st' <- E.handleEditorEvent e (inputField st)
  continue $ st { inputField = st' }
  
handleEvent st _ = continue st

-- Run the App
main :: IO ()
main = do
  _ <- defaultMain app initialInputState
  return ()

