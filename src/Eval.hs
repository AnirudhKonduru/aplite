{-# LANGUAGE FlexibleContexts #-}

module Eval (module Eval) where

import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.State (MonadState (..), State, runState)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Language as L
import MkParser (expressionParser, parse')

-- type Var = String

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


shapeOfValue :: L.Value -> Int
shapeOfValue (L.Array shape _) = head shape
shapeOfValue (L.Scalar _) = 0

isEValue :: L.Expression -> Bool
isEValue (L.EValue _) = True
isEValue _ = False


evalFunctionExpression :: L.FunctionExpression -> L.Function
evalFunctionExpression (L.BuiltInFunction _ f) = f
evalFunctionExpression (L.MOpF (L.MonadicOperator _ _ f) fexp) = f (evalFunctionExpression fexp)
evalFunctionExpression (L.DOpF (L.DyadicOperator _ _ f) fexp1 fexp2) = evalFunctionExpression fexp1 `f` evalFunctionExpression fexp2


-- | step evaluates a single expression
stepE :: (MonadError RunTimeError m, MonadState Store m) => L.Expression -> m L.Expression
stepE (L.EValue val) = do
  return $ L.EValue val

stepE (L.EVariable var) = do
  store <- get
  case Map.lookup var store of
    Just val -> return $ L.EValue val
    Nothing -> throwError $ UndefinedVariable "Undefined variable"

stepE (L.EArray exps) = do
  exps' <- mapM stepE exps
  case exps' of
    [] -> return $ L.EValue (L.Array [0] [])
    
    [e] -> do
      e' <- stepE e
      case e' of
        (L.EValue val) -> case val of
          (L.Scalar _) -> return $ L.EValue val
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
  return $ L.EValue val

stepE (L.EMonadic fexp e) = do
  exp' <- stepE e
  case exp' of
    (L.EValue val) -> return $ L.EValue (L.applyMonadic (evalFunctionExpression fexp) val)
    _ -> return $ L.EMonadic fexp exp'

stepE (L.EDyadic fexp exp1 exp2) = do
  exp1' <- stepE exp1
  exp2' <- stepE exp2
  case (exp1', exp2') of
    (L.EValue val1, L.EValue val2) -> return $ L.EValue (L.applyDyadic (evalFunctionExpression fexp) val1 val2)
    _ -> return $ L.EDyadic fexp exp1' exp2'


type M = ExceptT RunTimeError (State Store)

-- | step evaluates a single expression
step :: L.Expression -> Store -> (Either RunTimeError L.Expression, Store)
step = runState . runExceptT . stepE

final :: L.Expression -> Bool
final (L.EValue _) = True
final _ = False

-- | run
evalE :: L.Expression -> IO ()
evalE e = do
  let (result, _) = step e Map.empty
  case result of
    Left err -> print err
    Right (L.EValue val) -> print val
    Right expr -> evalE expr


dummyFunction :: L.Function
dummyFunction = L.AmbiguousF id const

-- take string, parse and return IO
run :: String -> IO ()
run = evalE . parse' expressionParser

-- >>> run "2 + 3"


