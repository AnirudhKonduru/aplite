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

data MonadicFunctionExpression
  = BuiltInMonadic String String MonadicFunction
  | MOpMf MonadicOperator MonadicFunctionExpression
  | MOpDf MonadicOperator DyadicFunctionExpression

instance Show MonadicFunctionExpression where
  show :: MonadicFunctionExpression -> String
  show (BuiltInMonadic _ name _) = name
  show (MOpMf mop mf) = show mop ++ "(" ++ show mf ++ ")"
  show (MOpDf mop df) = show mop ++ "(" ++ show df ++ ")"

instance Pretty MonadicFunctionExpression where
  pretty :: MonadicFunctionExpression -> Doc ann
  pretty (BuiltInMonadic symbol _ _) = pretty symbol
  pretty (MOpMf mop mf) = pretty mf PP.<> pretty mop
  pretty (MOpDf mop df) = pretty df PP.<> pretty mop

instance Eq MonadicFunctionExpression where
  (==) :: MonadicFunctionExpression -> MonadicFunctionExpression -> Bool
  (BuiltInMonadic _ name1 _) == (BuiltInMonadic _ name2 _) = name1 == name2
  (MOpMf mop1 mf1) == (MOpMf mop2 mf2) = mop1 == mop2 && mf1 == mf2
  (MOpDf mop1 df1) == (MOpDf mop2 df2) = mop1 == mop2 && df1 == df2
  _ == _ = False

-- applyMonadicFunction :: MonadicFunctionExpression ->

data DyadicFunctionExpression
  = BuiltInDyadic String String DyadicFunction
  | DOpDfDf DyadicOperator DyadicFunctionExpression DyadicFunctionExpression
  | DOpDfMf DyadicOperator DyadicFunctionExpression MonadicFunctionExpression
  -- | MonadicOp1 MonadicOperator DyadicFunctionExpression
  -- | DyadicOp DyadicOperator DyadicFunctionExpression DyadicFunctionExpression

instance Show DyadicFunctionExpression where
  show :: DyadicFunctionExpression -> String
  show (BuiltInDyadic _ name _) = name
  show (DOpDfDf dop df1 df2) = show df1 ++ show dop ++ show df2
  show (DOpDfMf dop df mf) = show df ++ show dop ++ show mf

instance Pretty DyadicFunctionExpression where
  pretty :: DyadicFunctionExpression -> Doc ann
  pretty (BuiltInDyadic symbol _ _) = pretty symbol
  pretty (DOpDfDf dop df1 df2) = pretty df1 PP.<> pretty dop PP.<> pretty df2
  pretty (DOpDfMf dop df mf) = pretty df PP.<> pretty dop PP.<> pretty mf

instance Eq DyadicFunctionExpression where
  (==) :: DyadicFunctionExpression -> DyadicFunctionExpression -> Bool
  (BuiltInDyadic _ name1 _) == (BuiltInDyadic _ name2 _) = name1 == name2
  (DOpDfDf dop1 df11 df12) == (DOpDfDf dop2 df21 df22) = dop1 == dop2 && df11 == df21 && df12 == df22
  (DOpDfMf dop1 df1 mf1) == (DOpDfMf dop2 df2 mf2) = dop1 == dop2 && df1 == df2 && mf1 == mf2
  _ == _ = False

data Expression
  = EVariable String
  | EValue Value
  | EMonadic MonadicFunctionExpression Expression
  | EDyadic DyadicFunctionExpression Expression Expression
  | EBind String Expression
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