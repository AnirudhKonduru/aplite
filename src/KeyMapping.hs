module KeyMapping (module KeyMapping) where

import Language

import Brick
import Brick.Widgets.Center ( center )
import Brick.Widgets.Border.Style ( unicode, unicodeBold )
import Brick.Widgets.Border ( border, borderWithLabel, vBorder )
import Brick.Widgets.Core 

import qualified Graphics.Vty as V
import qualified Brick.Widgets.Edit as E
import Brick.Widgets.Edit ( getEditContents )
import Control.Monad.Cont (MonadIO(liftIO))


data Name = Edit
          | EditLines
          deriving (Ord, Show, Eq)


data Key = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z 
          | Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
          | Alpha | Beta | Gamma | Delta | Epsilon | Zeta | Eta | Theta | Iota | Jot | Kappa | Lambda | Mu | Nu | Xi | Pi | Rho | Sigma | Tau | Upsilon 
          | Phi | Chi | Psi | Omega
          | Space | Backspace
            
            deriving (Eq, Ord)

newtype Keyboard = Keyboard [Key]

instance Eq Keyboard where
    (Keyboard k1) == (Keyboard k2) = k1 == k2
    (Keyboard k1) /= (Keyboard k2) = k1 /= k2


-- | Normal keyboard with alphabet
initKeyboard :: Keyboard
initKeyboard = Keyboard [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine, Space, Backspace]

-- | APL keyboard with apl characters
aplKeyboard :: Keyboard
aplKeyboard = Keyboard [Alpha, Beta, Gamma, Delta, Epsilon, Zeta, Eta, Theta, Iota, Jot, Kappa, Lambda, Mu, Nu, Xi, Pi, Rho, Sigma, Tau, Upsilon, Phi, Chi, Psi, Omega, Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine, Space, Backspace]

-- map from init keyboard to APL keyboard
mapKeyboard :: Keyboard -> Keyboard
mapKeyboard (Keyboard k) = Keyboard $ map (\x -> case x of
                                                  A -> Alpha
                                                  B -> Beta
                                                  C -> Gamma
                                                  D -> Delta
                                                  E -> Epsilon
                                                  F -> Zeta
                                                  G -> Eta
                                                  H -> Theta
                                                  I -> Iota
                                                  J -> Jot
                                                  K -> Kappa
                                                  K -> Lambda
                                                  L -> Mu
                                                  M -> Nu
                                                  N -> Xi
                                                  O -> Pi
                                                  P -> Rho
                                                  Q -> Sigma
                                                  R -> Tau
                                                  S -> Upsilon
                                                  T -> Phi
                                                  U -> Chi
                                                  V -> Psi
                                                  W -> Omega
                                                  _ -> x) k

-- | map from character to APL character
mapKeyToApl :: Char -> Char
mapKeyToApl kb = case kb of
                      'A' -> '←'
                      'a' -> '←'
                      'B' -> '|'
                      'b' -> '|'
                      'C' -> '_'
                      'c' -> '_'
                      'D' -> '÷'
                      'd' -> '÷'
                      'E' -> '¨'
                      'e' -> '¨'
                      'F' -> '⌊'
                      'f' -> '⌊'
                      'G' -> '⍟'
                      'g' -> '⍟'
                      'H' -> '≠'
                      'h' -> '≠'
                      'I' -> '⍳'
                      'i' -> '⍳'
                      'J' -> '_'
                      'j' -> '_'
                      'K' -> '_'  
                      'k' -> '_'
                      'L' -> '⍝'
                      'l' -> '⍝'
                      'M' -> '_'
                      'm' -> '_'
                      'N' -> '_'
                      'n' -> '_'
                      'O' -> '○'
                      'o' -> '○'
                      'P' -> '+'
                      'p' -> '+'
                      'Q' -> '_'
                      'q' -> '_'
                      'R' -> '⍴'
                      'r' -> '⍴'
                      'S' -> '¯'
                      's' -> '¯'
                      'T' -> '⌈'
                      't' -> '⌈'
                      'U' -> '_'
                      'u' -> '_'
                      'V' -> '_'
                      'v' -> '_'
                      'W' -> '_'
                      'w' -> '_'
                      'X' -> '_'
                      'x' -> '_'
                      'Y' -> '_'
                      'y' -> '_'
                      'Z' -> '_'
                      'z' -> '_'


-- | Render Keys as Brick Widgets
renderRegularButton :: Key -> Widget Name
renderRegularButton button =
  case button of
    A -> withBorderStyle unicode $ border $ str "A"
    B -> withBorderStyle unicode $ border $ str "B"
    C -> withBorderStyle unicode $ border $ str "C"
    D -> withBorderStyle unicode $ border $ str "D"
    E -> withBorderStyle unicode $ border $ str "E"
    F -> withBorderStyle unicode $ border $ str "F"
    G -> withBorderStyle unicode $ border $ str "G"
    H -> withBorderStyle unicode $ border $ str "H"
    I -> withBorderStyle unicode $ border $ str "I"
    J -> withBorderStyle unicode $ border $ str "J"
    K -> withBorderStyle unicode $ border $ str "K"
    L -> withBorderStyle unicode $ border $ str "L"
    M -> withBorderStyle unicode $ border $ str "M"
    N -> withBorderStyle unicode $ border $ str "N"
    O -> withBorderStyle unicode $ border $ str "O"
    P -> withBorderStyle unicode $ border $ str "P"
    Q -> withBorderStyle unicode $ border $ str "Q"
    R -> withBorderStyle unicode $ border $ str "R"
    S -> withBorderStyle unicode $ border $ str "S"
    T -> withBorderStyle unicode $ border $ str "T"
    U -> withBorderStyle unicode $ border $ str "U"
    V -> withBorderStyle unicode $ border $ str "V"
    W -> withBorderStyle unicode $ border $ str "W"
    X -> withBorderStyle unicode $ border $ str "X"
    Y -> withBorderStyle unicode $ border $ str "Y"
    Z -> withBorderStyle unicode $ border $ str "Z"
    Space -> withBorderStyle unicode $ border $ str "Space"
    Backspace -> withBorderStyle unicode $ border $ str "Backspace"
    Zero -> withBorderStyle unicode $ border $ str "0"
    One -> withBorderStyle unicode $ border $ str "1"
    Two -> withBorderStyle unicode $ border $ str "2"
    Three -> withBorderStyle unicode $ border $ str "3"
    Four -> withBorderStyle unicode $ border $ str "4"
    Five -> withBorderStyle unicode $ border $ str "5"
    Six -> withBorderStyle unicode $ border $ str "6"
    Seven -> withBorderStyle unicode $ border $ str "7"
    Eight -> withBorderStyle unicode $ border $ str "8"
    Nine -> withBorderStyle unicode $ border $ str "9"
    Alpha -> withBorderStyle unicode $ border $ str "A: ← [assign]"
    Beta -> withBorderStyle unicode $ border $ str "B: | [magnitude]"
    Gamma -> withBorderStyle unicode $ border $ str "C: _"
    Delta -> withBorderStyle unicode $ border $ str "D: ÷ [divide]"
    Epsilon -> withBorderStyle unicode $ border $ str "E: ¨ [foreach]"
    Zeta -> withBorderStyle unicode $ border $ str "F: ⌊ [floor]"
    Eta -> withBorderStyle unicode $ border $ str "G: ⍟ [natlog]"
    Theta -> withBorderStyle unicode $ border $ str "H: ≠ [not equal]"
    Iota -> withBorderStyle unicode $ border $ str "I: ⍳ [index]"
    Jot -> withBorderStyle unicode $ border $ str "J: _"
    Kappa -> withBorderStyle unicode $ border $ str "K: _"
    Lambda -> withBorderStyle unicode $ border $ str "L: ⍝ [comment]"
    Nu -> withBorderStyle unicode $ border $ str "O: ○ [natlog]"
    Xi -> withBorderStyle unicode $ border $ str "N: _"
    Mu -> withBorderStyle unicode $ border $ str "M: _"
    Rho -> withBorderStyle unicode $ border $ str "P: ⍴ [shape]"
    Sigma -> withBorderStyle unicode $ border $ str "Q: ⍉ [transpose]"
    Tau -> withBorderStyle unicode $ border $ str "R: _"
    Upsilon -> withBorderStyle unicode $ border $ str "S: ¯ [negate]"
    Phi -> withBorderStyle unicode $ border $ str "T: ⌈ [ceiling]"
    Chi -> withBorderStyle unicode $ border $ str "U: _"
    Psi -> withBorderStyle unicode $ border $ str "V: _"
    Omega -> withBorderStyle unicode $ border $ str "W: _"
    _ -> withBorderStyle unicode $ border $ str " "



chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = as : chunksOf n bs
  where (as,bs) = splitAt n xs

-- | Render Keyboard as Brick Widget
renderKeyboard :: Keyboard -> Widget Name
renderKeyboard (Keyboard buttons) = center $ vBox $ map hBox $ chunksOf 10 $ map renderRegularButton buttons


-- | Draw the Keyboard with borders and labels
drawGrid :: Keyboard -> Widget Name
drawGrid keyboard = withBorderStyle unicode $ borderWithLabel (str "Keyboard") $ center $ renderKeyboard keyboard

-- | Used for writing instructions to the UI (like how to quit, how to clear, etc.)
drawInstructions :: String -> Widget Name
drawInstructions instructions =  withBorderStyle unicode $ center $ border $ str instructions