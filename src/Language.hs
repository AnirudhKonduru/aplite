{-# LANGUAGE InstanceSigs #-}

module Language (module Language) where

import Data.List.Split (chunksOf)
import Prettyprinter (Doc, Pretty (pretty))
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.String as PP

-- function to pretty print anything that can be pretty printed
print :: Pretty a => a -> String
print = PP.renderString . PP.layoutPretty PP.defaultLayoutOptions . PP.pretty

-- APL function type. A function can be monadic, dyadic, or ambiguous
data Function
  = MonadicF MonadicFunction
  | DyadicF DyadicFunction
  | AmbiguousF MonadicFunction DyadicFunction

-- apply a function to a value as a monadic function.
-- If the function doesn't have a monadic form, throw an error
applyMonadic :: Function -> Value -> Value
applyMonadic (MonadicF f) v = f v
applyMonadic (DyadicF _) _ = error "rank error"
applyMonadic (AmbiguousF f _) v = f v

-- apply a function to two values as a dyadic function.
-- If the function doesn't have a dyadic form, throw an error
applyDyadic :: Function -> Value -> Value -> Value
applyDyadic (MonadicF _) _ _ = error "rank error"
applyDyadic (DyadicF f) v1 v2 = f v1 v2
applyDyadic (AmbiguousF _ f) v1 v2 = f v1 v2

-- Monadic functions are functions that take a single value and return a single value
type MonadicFunction = (Value -> Value)

-- Dyadic functions are functions that take two values and return a single value
type DyadicFunction = (Value -> Value -> Value)

{- APL Operator types.
The arguments to operators are functions, and the return value is a function,
 and these functions can be monadic or dyadic
 In other words, APL functions are higher order functions
 In each of these types, the first String is the symbol of the operator,
 the second String is the name of the operator
-}

-- APL monadic operators are functions that take a single function and return a single function
data MonadicOperator = MonadicOperator String String (Function -> Function)

-- APL dyadic operators are functions that take two functions and return a single function
data DyadicOperator = DyadicOperator String String (Function -> Function -> Function)


-- Show and Pretty instances for operators

instance (Show MonadicOperator) where
  show :: MonadicOperator -> String
  show (MonadicOperator _ name _) = name

instance (Pretty MonadicOperator) where
  pretty :: MonadicOperator -> Doc ann
  pretty (MonadicOperator symbol _ _) = pretty symbol

instance Eq MonadicOperator where
  (==) :: MonadicOperator -> MonadicOperator -> Bool
  (MonadicOperator _ name1 _) == (MonadicOperator _ name2 _) = name1 == name2

instance (Show DyadicOperator) where
  show :: DyadicOperator -> String
  show (DyadicOperator _ name _) = name

instance (Pretty DyadicOperator) where
  pretty :: DyadicOperator -> Doc ann
  pretty (DyadicOperator symbol _ _) = pretty symbol

instance Eq DyadicOperator where
  (==) :: DyadicOperator -> DyadicOperator -> Bool
  (DyadicOperator _ name1 _) == (DyadicOperator _ name2 _) = name1 == name2

-- APL scalar is a single value, and can be a number or a character
-- Note - we currently only support numbers
data Scalar
  = -- A scalar is either a single value (Int for now, we'll add more later)
    Number Float
  | Char Char
  deriving (Show, Eq)

-- An APL value is either a scalar or an array of values
-- APL arrays are multi-dimensional.
data Value
  = Scalar Scalar
  | -- The first [Int] is the shape of the array, it's length is the rank of the array
    -- The second [a] is the values of the array
    Array [Int] [Value]
  deriving (Show, Eq)

-- Pretty instance for Value types

nLines :: Int -> Doc ann
nLines n = PP.vcat (replicate n PP.line)

instance Pretty Value where
  pretty :: Value -> Doc ann
  pretty (Scalar (Number x)) = pretty x
  pretty (Scalar (Char x)) = pretty x
  -- print array, APL style
  pretty (Array [_] xs) = PP.align $ PP.hcat (PP.punctuate PP.space (map pretty xs))
  -- print the inner array, separated by newlines
  pretty arr@(Array shape _) = PP.align $ PP.vsep (PP.punctuate (nLines ((-) (length shape) 2)) (map pretty (unwound1dim arr)))

-- pp (Array s xs) = undefined

unwound1dim :: Value -> [Value]
unwound1dim (Scalar x) = [Scalar x]
unwound1dim (Array [] x) = x
unwound1dim (Array [_] xs) = xs
unwound1dim (Array (_ : as) xs) = map (Array as) (chunksOf (product as) xs)

-- APL operators, a non-exhaustive list
aplFunctions :: String
aplFunctions = "¨<≤=>≠∨∧×÷?⍵∊⍴~↑↓⍳○*←→⊢⍺⌈⌊_∇ ∆∘'⎕⍎⍕⊂⊥⊤|⍝!@#$%^&*_+-/:<>?"

aplMonadicOperators :: String
aplMonadicOperators = "/\\⌿⍀¨⍨"

aplDyadicOperators :: String
aplDyadicOperators = ".∘⍤⍥@"

-- APL functions can either be built in functions, 
-- or they can be composed of operators. We call these function expressions.
data FunctionExpression
  -- BuiltInFunction is a function that is built into the language
  -- The first String is the symbol of the function
  = BuiltInFunction String Function
  -- Applying the operator to the functions should result in a function
  | MOpF MonadicOperator FunctionExpression 
  | DOpF DyadicOperator FunctionExpression FunctionExpression

-- Show and Pretty instances for FunctionExpression
instance Show FunctionExpression where
  show :: FunctionExpression -> String
  show (BuiltInFunction symbol _) = symbol
  show (MOpF mop mf) = "(" ++ show mop ++ show mf ++ ")"
  show (DOpF dop f1 f2) = "(" ++ show f1 ++ show dop ++ show f2 ++ ")"

instance Pretty FunctionExpression where
  pretty :: FunctionExpression -> Doc ann
  pretty (BuiltInFunction symbol _) = pretty symbol
  pretty (MOpF mop mf) = pretty mf PP.<> pretty mop
  pretty (DOpF dop f1 f2) = pretty f1 PP.<> pretty dop PP.<> pretty f2

instance Eq FunctionExpression where
  (==) :: FunctionExpression -> FunctionExpression -> Bool
  (BuiltInFunction name1 _) == (BuiltInFunction name2 _) = name1 == name2
  (MOpF mop1 f1) == (MOpF mop2 f2) = mop1 == mop2 && f1 == f2
  (DOpF dop1 f11 f12) == (DOpF dop2 f21 f22) = dop1 == dop2 && f11 == f21 && f12 == f22
  _ == _ = False

-- Finally, we have the APL expression type
-- This is the type that we will parse into
data Expression
  = EVariable String
  -- Parse a value. This is always parsed into an array
  -- If the resulting array has only one element, the stepper will
  -- convert it into a scalar
  | EValue Value
  -- Parsing a expression with a monadic function
  | EMonadic FunctionExpression Expression
  -- Parsing a expression with a dyadic function
  | EDyadic FunctionExpression Expression Expression
  -- Bind operator is used to store a value in a variable
  -- NOTE: cuurently not implemented, although both the parser
  -- and the stepper are State monads, and can support this.
  | EBind String Value
  -- An Array of expressions, that evaluates to an array
  | EArray [Expression]
  deriving (Show, Eq)


-- pretty instance for Expression
instance Pretty Expression where
  pretty :: Expression -> Doc ann
  pretty (EVariable name) = pretty name
  pretty (EValue v) = pretty v
  pretty (EMonadic mf e) = pretty mf PP.<> pretty e
  pretty (EDyadic df e1 e2) = PP.parens (pretty e1) PP.<> pretty df PP.<> pretty e2
  pretty (EBind name e) = pretty name PP.<> pretty "←" PP.<> pretty e
  pretty (EArray es) = PP.brackets $ PP.hcat $ PP.punctuate PP.comma $ map pretty es

-- APL commands - these are not expressions, but rather commands
-- NOTE: currently not implemented, but the types are here for completeness
data Command = String

data Input
  = IExpression Expression
  | ICommand Command