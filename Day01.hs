#!/usr/bin/env stack
{- stack --resolver=lts-14.0 script -}
module Main
  ( main
  )
where

import           Data.Char

parse :: String -> [Int]
parse = map digitToInt

calc :: [Int] -> Int
calc is = go 0 is
 where
  go r (x : y : xs) | x == y    = go (r + x) (y : xs)
                    | otherwise = go r (y : xs)
  go r [x] | x == head is = r + x
  go r []                 = r

main :: IO ()
main = do
  res <- calc . parse <$> getLine
  print res
