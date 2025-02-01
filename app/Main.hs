module Main (main) where

import           Control.Monad.Fix     (fix)
import           Criterion.Main        (bench, bgroup, defaultMain, whnf)
import           Data.Function.Memoize (memoFix, memoize)

-- naive recursive version of the fibonacci function
fib :: Int -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- the nth fibonacci number can be looked up in the infinite list of fibonacci numbers (fibs) using the !! operator
fib1 :: Int -> Integer
fib1 n = fibs !! n

-- infinite list of fibonacci numbers this is a CAF (Constant Applicative Form)
fibs :: [Integer]
fibs = zipWith (+) (0 : (1 : fibs)) (1 : fibs)

-- memoized version of the fibonacci function:
-- we maintain a list of fibonacci numbers and look up the nth fibonacci number in the list
fib2 :: Int -> Integer
fib2 = (map fib' [0 ..] !!)
  where
    fib' 1 = 1
    fib' 2 = 1
    fib' n = fib2 (n - 2) + fib2 (n - 1)

-- using the fix fixpoint combinator (aka Y combinator)
-- short recap
-- fix :: (a -> a) -> a
-- fix f = f (fix f)

-- naive version of factorial
fac :: Integer -> Integer
fac 0 = 1
fac n = n * fac (n - 1)

-- a non-recursive version of the factorial function
facFix :: (Integer -> Integer) -> Integer -> Integer
facFix f 0 = 1
facFix f n = n * f (n - 1)

-- we can now define the factorial function using the fix combinator
fac1 :: Integer -> Integer
fac1 = fix facFix

-- we can also use the fix combinator to define the fibonacci function
fibFix :: (Int -> Integer) -> Int -> Integer
fibFix f 0 = 1
fibFix f 1 = 1
fibFix f n = f (n - 1) + f (n - 2)

fib3 :: Int -> Integer
fib3 = fix fibFix

-- memoization
-- we write our own memoization function
memoize' :: (Int -> a) -> (Int -> a)
memoize' f = (map f [0 ..] !!)

fib4 :: Int -> Integer
fib4 = fix (memoize' . fibFix)

-- instead of rolling our own memoization function, we can use the memoize function from the memoize package
fib5 :: Int -> Integer
fib5 = fix (memoize . fibFix)

-- memoFix is a combination of the fix combinator and the memoize function
fib6 :: Int -> Integer
fib6 = memoFix fibFix

-- let's run some benchmarks
main :: IO ()
main = do
  let n = 35
  defaultMain
    [ bgroup
        "fibonacci implementations"
        [ bench "fib" $ whnf fib n,
          bench "fib1" $ whnf fib1 n,
          bench "fib2" $ whnf fib2 n,
          bench "fib3" $ whnf fib3 n,
          bench "fib4" $ whnf fib4 n,
          bench "fib5" $ whnf fib5 n,
          bench "fib6" $ whnf fib6 n
        ]
    ]
