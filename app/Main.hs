{-# LANGUAGE OverloadedStrings #-} -- 1.
module Main where

type Start = Int
type End = Int
type Interval = Float
type Precision = Float

bisect :: Start -> End -> (Float -> Float) -> Interval -> Interval -> Precision -> Float
bisect s e f' ia ib p
  | s >= e = -1.0
  | otherwise = applyFunction
  where
    c :: Float
    c = (ia + ib) / 2.0
    applyFunction =
      if f' c == 0 || (ib - ia) / 2.0 < p then c
      else
        if signum (f' c) == signum (f' ia) then bisect (s + 1) e f' c ib p
        else bisect (s + 1) e f' ia c p
          

f :: Float -> Float
f x = x ^ (2 :: Integer) - x - 6.0

main :: IO ()
main = putStrLn "Hello, Haskell!"
