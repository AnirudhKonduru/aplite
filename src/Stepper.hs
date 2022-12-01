{-# LANGUAGE FlexibleContexts #-}

module Stepper (module Stepper) where

import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.State (MonadState (..), State, runState)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Language as L

type Var = String

type Store = Map Var L.Value

data RunTimeError
  = LengthError String
  | RankError String
  | ShapeError String
  | TypeMismatch String
  | UndefinedVariable String
  | UnknownError String
  | UnimplementedError String
  deriving (Show, Eq)

-- | step evaluates a single expression
stepE :: (MonadError RunTimeError m, MonadState Store m) => L.Expression -> m L.Expression
stepE (L.Value val) = do
  return $ L.Value val
stepE (L.Variable var) = do
  store <- get
  case Map.lookup var store of
    Just val -> return $ L.Value val
    Nothing -> throwError $ UndefinedVariable var
stepE _ = do
  throwError $ UnimplementedError "Not implemented"

type M = ExceptT RunTimeError (State Store)

-- | step evaluates a single expression, monadic
step :: L.Expression -> Store -> (Either RunTimeError L.Expression, Store)
step = runState . runExceptT . stepE

final :: L.Expression -> Bool
final (L.Value _) = True
final _ = False

-- | run until the expression is a value
evalE :: L.Expression -> M L.Value
evalE e = do
  if final e
    then return $ extractValue e
    else evalE e
  where
    extractValue (L.Value v) = v
    extractValue _ = error "Not a value"

-- run step my step until the expression is a value
execute :: L.Expression -> Store -> (Either RunTimeError L.Value, Store)
execute = runState . runExceptT . evalE

runE :: L.Expression -> IO ()
runE e = print (fst (execute e Map.empty))

-- -- | run a program
-- run :: [L.Expression] -> IO ()
-- run = mapM_ runE

-- | run a program, monadic (defaults to IO?)
runM :: [L.Expression] -> M ()
runM = mapM_ evalE