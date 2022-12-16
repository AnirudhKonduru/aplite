{-# LANGUAGE InstanceSigs #-}

module Language (module Language) where

import Data.List.Split (chunksOf)
import Prettyprinter (Doc, Pretty (pretty))
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.String as PP

-- data Type = Value | MonadicFunction | DyadicFunction deriving (Show, Eq)

print :: Pretty a => a -> String
print = PP.renderString . PP.layoutPretty PP.defaultLayoutOptions . PP.pretty

data Function
  = MonadicF MonadicFunction
  | DyadicF DyadicFunction
  | AmbiguousF MonadicFunction DyadicFunction

applyMonadic :: Function -> Value -> Value
applyMonadic (MonadicF f) v = f v
applyMonadic (DyadicF _) _ = error "rank error"
applyMonadic (AmbiguousF f _) v = f v

applyDyadic :: Function -> Value -> Value -> Value
applyDyadic (MonadicF _) _ _ = error "rank error"
applyDyadic (DyadicF f) v1 v2 = f v1 v2
applyDyadic (AmbiguousF _ f) v1 v2 = f v1 v2

type MonadicFunction = (Value -> Value)

type DyadicFunction = (Value -> Value -> Value)

data MonadicOperator = MonadicOperator String String (Function -> Function)

data DyadicOperator = DyadicOperator String String (Function -> Function -> Function)

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

-- type Array a = Matrix a { row :: 1 }
newtype Monadic = MSym Char
  deriving (Show, Eq)

newtype Dyadic = DSym Char
  deriving (Show, Eq)

-- data TypedExpression = TypedExpression Type Expression

-- ofType :: Type -> Expression -> TypedExpression
-- t `ofType` e = TypedExpression t e

data FunctionExpression
  = BuiltInFunction String Function
  | MOpF MonadicOperator FunctionExpression
  | DOpF DyadicOperator FunctionExpression FunctionExpression

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

data Expression
  = EVariable String
  | EValue Value
  | EMonadic FunctionExpression Expression
  | EDyadic FunctionExpression Expression Expression
  | EBind String Value
  | EArray [Expression]
  deriving (Show, Eq)

instance Pretty Expression where
  pretty :: Expression -> Doc ann
  pretty (EVariable name) = pretty name
  pretty (EValue v) = pretty v
  pretty (EMonadic mf e) = pretty mf PP.<> pretty e
  pretty (EDyadic df e1 e2) = PP.parens (pretty e1) PP.<> pretty df PP.<> pretty e2
  pretty (EBind name e) = pretty name PP.<> pretty "←" PP.<> pretty e
  pretty (EArray es) = PP.brackets $ PP.hcat $ PP.punctuate PP.comma $ map pretty es

-- deriving (Show, Eq)

data Command = String

data Input
  = IExpression Expression
  | ICommand Command