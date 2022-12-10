module Language (module Language) where

data Type = Value | MonadicFunction | DyadicFunction deriving (Show, Eq)

data Function
  = MonadicF MonadicFunction
  | DyadicF DyadicFunction

-- define typeclass for monadic functions
class MonadicF a where
  func :: a -> MonadicFunction
  name :: a -> String
  symbol :: a -> String

type MonadicFunction = Value -> Value

type DyadicFunction = Value -> Value -> Value

type MonadicOperator = Function -> Function

type DyadicOperator = Function -> Function -> Function

data Scalar
  = -- A scalar is either a single value (Int for now, we'll add more later)
    Number Float
  | Char Char
  deriving (Show, Eq)

data Value
  = Scalar Scalar
  | -- The first [Int] is the shape of the array, it's length is the rank of the array
    -- The second [a] is the values of the array
    Array [Int] [Value]
  deriving (Show, Eq)

-- APL operators, a non-exhaustive list
aplFunctions :: String
aplFunctions = "¨<≤=>≠∨∧×÷?⍵∊⍴~↑↓⍳○*←→⊢⍺⌈⌊_∇ ∆∘'⎕⍎⍕⊂⊥⊤|⍝!@#$%^&*_+-/:<>?"

aplMonadicOperators :: String
aplMonadicOperators = "/\\⌿⍀¨⍨"

aplDyadicOperators :: String
aplDyadicOperators = ".∘⍤⍥@"

-- type Array a = Matrix a { row :: 1 }
newtype Monadic = MSym Char
  deriving (Show, Eq)

newtype Dyadic = DSym Char
  deriving (Show, Eq)

data TypedExpression = TypedExpression Type Expression

ofType :: Type -> Expression -> TypedExpression
t `ofType` e = TypedExpression t e

data MonadicFunctionExpression
  = BuiltInMonadic String String MonadicFunction
  | MonadicOp MonadicOperator MonadicFunctionExpression

-- instance Show MonadicFunctionExpression where
--   show (BuiltInMonadic _ name _) = name
--   show (MonadicOp mop mf) = "(" ++ show mf ++ ")" ++ show mop

-- applyMonadicFunction :: MonadicFunctionExpression ->

data DyadicFunctionExpression
  = BuiltInDyadic DyadicFunction
  | MonadicOp1 MonadicOperator DyadicFunctionExpression
  | DyadicOp DyadicOperator DyadicFunctionExpression DyadicFunctionExpression

-- data FunctionExpression =
--   BuiltIn Function
--   | MonadicOp1 MonadicOperator FunctionExpression
--   | MonadicOp2 MonadicOperator FunctionExpression FunctionExpression
--   | DyadicOp DyadicOperator FunctionExpression FunctionExpression

data Expression
  = EVariable String
  | EValue Value
  | EMonadic MonadicFunctionExpression Expression
  | EDyadic DyadicFunctionExpression Expression Expression
  | EBind String Expression
  | EArray [Expression]

-- deriving (Show, Eq)

data Command = String

data Input
  = IExpression Expression
  | ICommand Command