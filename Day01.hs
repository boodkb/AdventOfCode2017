#!/usr/bin/env stack
{- stack --resolver=lts-14.0 script -}
module Main
  ( main
  )
where

import           Data.Char

parse :: String -> [Int]
parse = map digitToInt

calc1 :: [Int] -> Int
calc1 is = go 0 is
 where
  go r (x : y : xs) | x == y    = go (r + x) (y : xs)
                    | otherwise = go r (y : xs)
  go r [x] | x == head is = r + x
  go r []                 = r

calc2 :: [Int] -> Int
calc2 [] = 0
calc2 is = sum $ take l $ zipWith f cycled shifted
 where
  cycled  = cycle is
  shifted = drop (l `div` 2) cycled
  l       = length is
  f i j = if i == j then i else 0

main :: IO ()
main = do
  is <- parse <$> getLine
  print $ calc1 is
  print $ calc2 is
