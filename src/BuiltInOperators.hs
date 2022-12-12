{-# LANGUAGE LambdaCase #-}

module BuiltInOperators (module BuiltInOperators) where

import Data.List.Split
import Language

-- Monadic operators

reduce :: Function -> Function
reduce f = MonadicF $ \case
  Scalar s -> Scalar s
  arr@(Array shape xs) -> do
    let n = last shape
    if n == 1
      then arr
      else Array (init shape) (map (foldl1 (applyDyadic f)) $ chunksOf n xs)

scan :: Function -> Function
scan f = MonadicF $ \case
  Scalar s -> Scalar s
  arr@(Array shape xs) -> do
    let n = last shape
    if n == 1
      then arr
      else Array (init shape) (concatMap (scanl1 (applyDyadic f)) (chunksOf n xs))

-- Dyadic operators

product :: Function -> Function -> Function
product f1 f2 = DyadicF $ \x y ->
  case (x, y) of
    (Scalar s1, Scalar s2) -> applyDyadic f2 (Scalar s1) (Scalar s2)
    (Scalar s1, Array shape xs) -> do
      let n = last shape
      let firstOp = map (applyDyadic f2 (Scalar s1)) xs
      Array (init shape) (map (foldl1 (applyDyadic f1)) $ chunksOf n firstOp)
    (Array shape xs, Scalar s2) -> do
      let n = last shape
      let firstOp = map (\x' -> applyDyadic f2 x' (Scalar s2)) xs
      Array (init shape) (map (foldl1 (applyDyadic f1)) $ chunksOf n firstOp)
    (Array shape1 xs, Array shape2 ys) | shape1 == shape2 -> Array shape1 (zipWith (applyDyadic f2) xs ys)
    (Array shape1 _, Array shape2 _) -> error ("Shapes do not match: " ++ show shape1 ++ " and " ++ show shape2)
