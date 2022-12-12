module UIHelper (module UIHelper) where

import Language ( aplOperators )
import qualified Parser as P
-- import qualified Stepper as S

import Brick
import Brick.Widgets.Center ( center )
import Brick.Widgets.Border.Style ( unicode, unicodeBold )
import Brick.Widgets.Border ( border, borderWithLabel, vBorder )
import Brick.Widgets.Core 

import qualified Graphics.Vty as V
import qualified Brick.Widgets.Edit as E
import Brick.Widgets.Edit ( getEditContents )
import Control.Monad.Cont (MonadIO(liftIO))


data Key = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z 
          | Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
          | Alpha | Beta | Gamma | Delta | Epsilon | Zeta | Eta | Theta | Iota | Kappa | Lambda | Mu | Nu | Xi | Pi | Rho | Sigma | Tau | Upsilon 
          | Phi | Chi | Psi | Omega
          | Space | Backspace
            
            deriving (Eq, Ord)

newtype Keyboard = Keyboard [Key]

instance Eq Keyboard where
    (Keyboard k1) == (Keyboard k2) = k1 == k2
    (Keyboard k1) /= (Keyboard k2) = k1 /= k2


-- Create a function to initialize the keyboard with default state
initKeyboard :: Keyboard
initKeyboard = Keyboard [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine, Space, Backspace]

extraKeyboard :: Keyboard
extraKeyboard = Keyboard [Alpha, Beta, Gamma, Delta, Epsilon, Zeta, Eta, Theta, Iota, Kappa, Lambda, Mu, Nu, Xi, Pi, Rho, Sigma, Tau, Upsilon, Phi, Chi, Psi, Omega, Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine, Space, Backspace]

-- map from init keyboard to extra keyboard
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
                                                  J -> Kappa
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

mapKeyToGreek :: Char -> Char
mapKeyToGreek kb = case kb of
                      'A' -> 'α'
                      'B' -> 'β'
                      'C' -> 'γ'
                      'D' -> 'δ'
                      'E' -> 'ε'
                      'F' -> 'ζ'
                      'G' -> 'η'
                      'H' -> 'θ'
                      'I' -> 'ι'
                      'J' -> 'κ'
                      'K' -> 'λ'
                      'L' -> 'μ'
                      'M' -> 'ν'
                      'N' -> 'ξ'
                      'O' -> 'π'
                      'P' -> 'ρ'
                      'Q' -> 'σ'
                      'R' -> 'τ'
                      'S' -> 'υ'
                      'T' -> 'φ'
                      'U' -> 'χ'
                      'V' -> 'ψ'
                      'W' -> 'ω'
                      'X' -> 'Α'
                      'Y' -> 'Β'
                      'Z' -> 'Γ'
                      _ -> ' '

buttonAttr :: AttrName
buttonAttr = attrName "button"

-- Create a function to render a button as a widget
renderRegularButton :: Key -> Widget ()
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
    Alpha -> withBorderStyle unicode $ border $ str "α"
    Beta -> withBorderStyle unicode $ border $ str "β"
    Sigma -> withBorderStyle unicode $ border $ str "σ"
    Rho -> withBorderStyle unicode $ border $ str "ρ"
    Pi -> withBorderStyle unicode $ border $ str "π"
    Xi -> withBorderStyle unicode $ border $ str "ξ"
    Nu -> withBorderStyle unicode $ border $ str "ν"
    Mu -> withBorderStyle unicode $ border $ str "μ"
    Lambda -> withBorderStyle unicode $ border $ str "λ"
    Kappa -> withBorderStyle unicode $ border $ str "κ"
    Iota -> withBorderStyle unicode $ border $ str "ι"
    Theta -> withBorderStyle unicode $ border $ str "θ"
    Eta -> withBorderStyle unicode $ border $ str "η"
    Zeta -> withBorderStyle unicode $ border $ str "ζ"
    Epsilon -> withBorderStyle unicode $ border $ str "ε"
    Delta -> withBorderStyle unicode $ border $ str "δ"
    Gamma -> withBorderStyle unicode $ border $ str "γ"
    Omega -> withBorderStyle unicode $ border $ str "ω"
    Phi -> withBorderStyle unicode $ border $ str "φ"
    Chi -> withBorderStyle unicode $ border $ str "χ"
    Psi -> withBorderStyle unicode $ border $ str "ψ"
    Upsilon -> withBorderStyle unicode $ border $ str "υ"
    Tau -> withBorderStyle unicode $ border $ str "τ"
    _ -> withBorderStyle unicode $ border $ str ""


chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = as : chunksOf n bs
  where (as,bs) = splitAt n xs


renderKeyboard :: Keyboard -> Widget ()
renderKeyboard (Keyboard buttons) = center $ vBox $ map hBox $ chunksOf 10 $ map renderRegularButton buttons


-- draw keyboard with borders in key board layout
-- draw border around every cell
drawGrid :: Keyboard -> Widget ()
drawGrid keyboard = withBorderStyle unicode $ borderWithLabel (str "Keyboard") $ center $ renderKeyboard keyboard


drawInstructions :: String -> Widget ()
drawInstructions instructions =  withBorderStyle unicode $ center $ borderWithLabel (str "") $ str instructions