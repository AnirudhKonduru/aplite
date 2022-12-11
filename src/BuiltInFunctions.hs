module BuiltInFunctions (module BuiltInFunctions) where

import Language

makeMonadicFunction :: (Float -> Float) -> (Value -> Value)
makeMonadicFunction f (Scalar (Number x)) = Scalar (Number (f x))
makeMonadicFunction f (Array shape xs) = Array shape (map (makeMonadicFunction f) xs)
makeMonadicFunction _f (Scalar (Char _)) = error "domain error"

makeDyadicFunction :: (Float -> Float -> Float) -> (Value -> Value -> Value)
makeDyadicFunction _f (Scalar (Number x)) (Scalar (Number y)) = Scalar (Number (x + y))
makeDyadicFunction f (Scalar (Number x)) (Array shape xs) = Array shape (map (makeDyadicFunction f (Scalar (Number x))) xs)
makeDyadicFunction f (Array shape xs) (Scalar (Number x)) = Array shape (map (flip (makeDyadicFunction f) (Scalar (Number x))) xs)
makeDyadicFunction f (Array shape1 xs) (Array shape2 ys) | shape1 == shape2 = Array shape1 (zipWith (makeDyadicFunction f) xs ys)
makeDyadicFunction _f (Array shape1 _) (Array shape2 _) = error ("Shapes do not match: " ++ show shape1 ++ " and " ++ show shape2)
makeDyadicFunction _f (Scalar (Char _)) _ = error "domain error"
makeDyadicFunction _f _ (Scalar (Char _)) = error "domain error"

shapeOf :: Value -> Value
shapeOf (Scalar (Number _)) = Scalar (Number 0)
shapeOf (Array s _) = Array [length s] (map ((Scalar . Number) . fromIntegral) s)
shapeOf (Scalar (Char _)) = Scalar (Number 0)

generateN :: Int -> [Value] -> [Value]
generateN n xs = take n (cycle xs)

getInt :: Value -> Either String Int
getInt (Scalar (Number x)) = Right $ round x
getInt (Scalar (Char _)) = Left "domain error"
getInt (Array _ _) = Left "domain error"

valueToShape :: [Value] -> Either String [Int]
valueToShape [] = Left "domain error"
valueToShape xs = do
  let s' = map getInt xs
  sequence s'

reshape :: Value -> Value -> Value
reshape (Scalar (Number s)) (Scalar v) = do
  let count = round s
  Array [count] (generateN count [Scalar v])
reshape (Scalar (Number s)) (Array _ xs) = do
  let count = round s
  Array [count] (generateN count xs)
reshape (Array _ xs) (Scalar v) = do
  let s' = valueToShape xs
  case s' of
    Left err -> error err
    Right s -> Array s (generateN (product s) [Scalar v])
reshape (Array [_] xs) (Array _ ys) = do
  let s' = valueToShape xs
  case s' of
    Left err -> error err
    Right s -> Array s (generateN (product s) ys)
reshape (Array [] _) _ = error "domain error"
reshape (Array (_ : _) _) _ = error "domain error"
reshape (Scalar (Char _)) _ = error "domain error"
