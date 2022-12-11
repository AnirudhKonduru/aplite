module BuiltInFunctions (module BuiltInFunctions) where

import Language
import Math.Gamma (gamma)
import Prelude hiding (min, max, floor, negate)
import qualified Prelude

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

reshape' :: Value -> Value -> Value
reshape' (Scalar (Number s)) (Scalar v) = do
  let count = round s
  Array [count] (generateN count [Scalar v])
reshape' (Scalar (Number s)) (Array _ xs) = do
  let count = round s
  Array [count] (generateN count xs)
reshape' (Array _ xs) (Scalar v) = do
  let s' = valueToShape xs
  case s' of
    Left err -> error err
    Right s -> Array s (generateN (product s) [Scalar v])
reshape' (Array [_] xs) (Array _ ys) = do
  let s' = valueToShape xs
  case s' of
    Left err -> error err
    Right s -> Array s (generateN (product s) ys)
reshape' (Array [] _) _ = error "domain error"
reshape' (Array (_ : _) _) _ = error "domain error"
reshape' (Scalar (Char _)) _ = error "domain error"

-- | all the built in monadic functions

conjugate :: MonadicFunctionExpression
conjugate = BuiltInMonadic "+" "conjugate" (makeMonadicFunction (0+))
negate :: MonadicFunctionExpression
negate = BuiltInMonadic "-" "negate" (makeMonadicFunction (0-))
direction :: MonadicFunctionExpression
direction = BuiltInMonadic "×" "direction" (makeMonadicFunction signum)
reciprocal :: MonadicFunctionExpression
reciprocal = BuiltInMonadic "÷" "reciprocal" (makeMonadicFunction (1/))
ciel :: MonadicFunctionExpression
ciel = BuiltInMonadic "⌈" "ciel" (makeMonadicFunction (toEnum . ceiling))
floor :: MonadicFunctionExpression
floor = BuiltInMonadic "⌊" "floor" (makeMonadicFunction (toEnum . Prelude.floor))
natlog :: MonadicFunctionExpression
natlog = BuiltInMonadic "⍟" "natlog" (makeMonadicFunction log)
exponential :: MonadicFunctionExpression
exponential = BuiltInMonadic "*" "exponential" (makeMonadicFunction exp)
pitimes :: MonadicFunctionExpression
pitimes = BuiltInMonadic "○" "pitimes" (makeMonadicFunction (* pi))
factorial :: MonadicFunctionExpression
factorial = BuiltInMonadic "!" "factorial" (makeMonadicFunction gamma)
roll :: MonadicFunctionExpression
roll = BuiltInMonadic "?" "roll" (makeMonadicFunction (1.0 -))
shape :: MonadicFunctionExpression
shape = BuiltInMonadic "⍴" "shape" shapeOf

-- | all the built in monadic functions, in a list

builtInMonadicFunctions :: [MonadicFunctionExpression]
builtInMonadicFunctions = [conjugate, negate, direction, reciprocal, ciel, floor, natlog, exponential, pitimes, factorial, roll, shape]

-- | all the built in dyadic function definitions

plus :: DyadicFunctionExpression
plus = BuiltInDyadic "+" "plus" (makeDyadicFunction (+))
minus :: DyadicFunctionExpression
minus = BuiltInDyadic "-" "minus" (makeDyadicFunction (-))
times :: DyadicFunctionExpression
times = BuiltInDyadic "×" "times" (makeDyadicFunction (*))
divide :: DyadicFunctionExpression
divide = BuiltInDyadic "÷" "divide" (makeDyadicFunction (/))
max :: DyadicFunctionExpression
max = BuiltInDyadic "⌈" "max" (makeDyadicFunction Prelude.max)
min :: DyadicFunctionExpression
min = BuiltInDyadic "⌊" "min" (makeDyadicFunction Prelude.min)
reshape :: DyadicFunctionExpression
reshape = BuiltInDyadic "⍴" "reshape" reshape'

-- | List of all the builtin dyadic functions
builtInDyadicFunctions :: [DyadicFunctionExpression]
builtInDyadicFunctions = [plus, minus, times, divide, max, min, reshape]