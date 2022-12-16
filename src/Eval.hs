{-# LANGUAGE FlexibleContexts #-}

module Eval (module Eval) where

import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.State (MonadState (..), State, runState)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Language as L
import Language (print)
import MkParser (expressionParser, parse')


-- | to Store variable names and values
type Store = Map String L.Value

data RunTimeError
  = LengthError String
  | RankError String
  | ShapeError String
  | TypeMismatch String
  | UndefinedVariable String
  | UnknownError String
  | UnimplementedError String
  deriving (Show, Eq)


-- | shape of a value (0 for scalars, array [shape] for arrays)
shapeOfValue :: L.Value -> Int
shapeOfValue (L.Array shape _) = head shape
shapeOfValue (L.Scalar _) = 0

-- | Check if an expression is of type value
isEValue :: L.Expression -> Bool
isEValue (L.EValue _) = True
isEValue _ = False

-- | Convert arrays of size 1 to scalars 
arrayToScalar :: L.Value -> L.Value
arrayToScalar (L.Array [1] [v]) = v
arrayToScalar v = v

{-
Evaluate a function expression recursivesly until it is a function
This is used to extract functions from function expressions which is then passed to applyMonadic or applyDyadic, 
which are applied on values to give the final result
-}
evalFunctionExpression :: L.FunctionExpression -> L.Function
evalFunctionExpression (L.BuiltInFunction _ f) = f
evalFunctionExpression (L.MOpF (L.MonadicOperator _ _ f) fexp) = f (evalFunctionExpression fexp)
evalFunctionExpression (L.DOpF (L.DyadicOperator _ _ f) fexp1 fexp2) = evalFunctionExpression fexp1 `f` evalFunctionExpression fexp2


-- | step evaluates a single expression
stepE :: (MonadError RunTimeError m, MonadState Store m) => L.Expression -> m L.Expression
stepE (L.EValue val) = do
  return $ L.EValue (arrayToScalar val)

stepE (L.EVariable var) = do
  store <- get
  case Map.lookup var store of
    Just val -> return $ L.EValue (arrayToScalar val)
    Nothing -> throwError $ UndefinedVariable "Undefined variable"

stepE (L.EArray exps) = do
  exps' <- mapM stepE exps
  case exps' of
    [] -> return $ L.EValue (L.Array [0] [])
    
    [e] -> do
      e' <- stepE e
      case e' of
        (L.EValue val) -> case val of
          (L.Scalar _) -> return $ L.EValue (arrayToScalar val)
          (L.Array shape vals) -> return $ L.EValue (L.Array (1 : shape) vals)
        _ -> return $ L.EArray [e']
    
    (e : es) -> if all isEValue (e : es)
      then return $ L.EValue (L.Array [length exps'] (map extractValue (e : es)))
      else return $ L.EArray exps'
    where 
      extractValue (L.EValue v) = v
      extractValue _ = error "Expected a value"

stepE (L.EBind var val) = do
  store <- get
  put $ Map.insert var val store
  return $ L.EValue (arrayToScalar val)

stepE (L.EMonadic fexp e) = do
  exp' <- stepE e
  case exp' of
    (L.EValue val) -> return $ L.EValue $ arrayToScalar (L.applyMonadic (evalFunctionExpression fexp) val)
    _ -> return $ L.EMonadic fexp exp'

stepE (L.EDyadic fexp exp1 exp2) = do
  exp1' <- stepE exp1
  exp2' <- stepE exp2
  case (exp1', exp2') of
    (L.EValue val1, L.EValue val2) -> return $ L.EValue $ arrayToScalar (L.applyDyadic (evalFunctionExpression fexp) val1 val2)
    _ -> return $ L.EDyadic fexp exp1' exp2'


-- | step evaluates a single expression and returns the new expression and the updated store
step :: L.Expression -> Store -> (Either RunTimeError L.Expression, Store)
step = runState . runExceptT . stepE


-- | evalE' evaluates an expression until it is a value and outputs that value
evalE' :: L.Expression -> L.Value
evalE' e = case step e Map.empty of
  (Left err, _) -> error $ show err
  (Right (L.EValue val), _) -> val
  (Right expr, _) -> evalE' expr


-- | evalE evaluates an expression until it is a value and outputs that value as a String
evalE :: L.Expression -> String
evalE e = case step e Map.empty of
  (Left err, _) -> show err
  (Right (L.EValue val), _) -> Language.print (L.EValue val)
  (Right expr, _) -> evalE expr

{- 
run takes a string as an input, parses and interprets the input, and returns the result as a string
-}
run :: String -> String
run = evalE . parse' expressionParser





