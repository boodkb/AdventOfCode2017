#!/usr/bin/env stack
{- stack --resolver=lts-14.0 script -}
module Main
  ( main
  )
where

import           Data.Char
import           Control.Monad

parseLine :: String -> [Int]
parseLine = map read . words

crc1 :: [[Int]] -> Int
crc1 = sum . map calc
 where
  calc [] = 0
  calc is = maximum is - minimum is

crc2 :: [[Int]] -> Int
crc2 sheet = sum $ f <$> sheet
 where
  f row = head $ do
    i <- row
    j <- row
    let (d, r) = i `quotRem` j
    guard (i /= j && r == 0)
    pure d



main :: IO ()
main = do
  sheet <- map parseLine . lines <$> getContents
  print $ crc1 sheet
  print $ crc2 sheet



