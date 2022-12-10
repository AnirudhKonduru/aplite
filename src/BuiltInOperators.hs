{-# LANGUAGE LambdaCase #-}

module BuiltInOperators (module BuiltInOperators) where

import Data.List.Split
import Language

-- Monadic operators

reduce :: MonadicOperator
reduce (MonadicF _) = error "reduce: requires a dyadic function"
reduce (DyadicF f) = MonadicF $ \case
  Scalar s -> Scalar s
  Array shape xs -> do
    let n = last shape
    Array (init shape) (map (foldl1 f) $ chunksOf n xs)

scan :: MonadicOperator
scan (MonadicF _) = error "scan: requires a dyadic function"
scan (DyadicF f) = MonadicF $ \case
  Scalar s -> Scalar s
  Array shape xs -> do
    let n = last shape
    Array (init shape) (concatMap (scanl1 f) (chunksOf n xs))

-- Dyadic operators

product :: DyadicOperator
product (DyadicF d1) (DyadicF d2) = DyadicF $ \x y ->
  case (x, y) of
    (Scalar s1, Scalar s2) -> d2 (Scalar s1) (Scalar s2)
    (Scalar s1, Array shape xs) -> do
      let n = last shape
      let firstOp = map (d2 (Scalar s1)) xs
      Array (init shape) (map (foldl1 d1) $ chunksOf n firstOp)
    (Array shape xs, Scalar s2) -> do
      let n = last shape
      let firstOp = map (flip d2 (Scalar s2)) xs
      Array (init shape) (map (foldl1 d1) $ chunksOf n firstOp)
    (Array shape1 xs, Array shape2 ys) | shape1 == shape2 -> Array shape1 (zipWith d2 xs ys)
    (Array shape1 _, Array shape2 _) -> error ("Shapes do not match: " ++ show shape1 ++ " and " ++ show shape2)
product _ _ = error "product: requires two dyadic functions"
