module Language (module Language) where

data Scalar
  = -- A scalar is either a single value (Int for now, we'll add more later)
    IntVal Int
  | FloatVal Float
  | -- or an arbitrary Value Boxed into a scalar
    Box Value
  deriving (Show, Eq)

data Value
  = Scalar Scalar
  | -- The first [Int] is the shape of the array, it's length is the rank of the array
    -- The second [a] is the values of the array
    Array [Int] [Value]
  deriving (Show, Eq)

-- APL operators, a non-exhaustive list
aplOperators :: String
aplOperators = "¨<≤=>≠∨∧×÷?⍵∊⍴~↑↓⍳○*←→⊢⍺⌈⌊_∇ ∆∘'⎕⍎⍕⊂⊥⊤|⍝⍀⌿!@#$%^&*_+-/:<>?"

-- type Array a = Matrix a { row :: 1 }
newtype Monadic = MSym Char
  deriving (Show, Eq)

newtype Dyadic = DSym Char
  deriving (Show, Eq)

data Expression
  = EVariable String
  | EValue Value
  | EMonadic Monadic Expression
  | EDyadic Expression Dyadic Expression
  | EBind String Expression
  | EArray [Expression]
  deriving (Show, Eq)

data Command = String

data Input
  = IExpression Expression
  | ICommand Command