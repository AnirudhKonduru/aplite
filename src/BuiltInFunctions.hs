module BuiltInFunctions (module BuiltInFunctions) where

import Language

makeMonadicFunction :: (Float -> Float) -> MonadicFunction
makeMonadicFunction f (Scalar (Number x)) = Scalar (Number (f x))
makeMonadicFunction f (Array shape xs) = Array shape (map (makeMonadicFunction f) xs)
makeMonadicFunction _f (Scalar (Char _)) = error "domain error"

makeDyadicFunction :: (Float -> Float -> Float) -> DyadicFunction
makeDyadicFunction _f (Scalar (Number x)) (Scalar (Number y)) = Scalar (Number (x + y))
makeDyadicFunction f (Scalar (Number x)) (Array shape xs) = Array shape (map (makeDyadicFunction f (Scalar (Number x))) xs)
makeDyadicFunction f (Array shape xs) (Scalar (Number x)) = Array shape (map (flip (makeDyadicFunction f) (Scalar (Number x))) xs)
makeDyadicFunction f (Array shape1 xs) (Array shape2 ys) | shape1 == shape2 = Array shape1 (zipWith (makeDyadicFunction f) xs ys)
makeDyadicFunction _f (Array shape1 _) (Array shape2 _) = error ("Shapes do not match: " ++ show shape1 ++ " and " ++ show shape2)
makeDyadicFunction _f (Scalar (Char _)) _ = error "domain error"
makeDyadicFunction _f _ (Scalar (Char _)) = error "domain error"
