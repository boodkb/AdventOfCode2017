#!/usr/bin/env stack
{- stack --resolver=lts-14.0 script --package=attoparsec -}
{-# LANGUAGE OverloadedStrings,RecordWildCards,BangPatterns #-}
module Main
  ( main
  )
where

import           Data.Function
import           Data.Bits
import qualified Data.ByteString               as B
import           Data.Attoparsec.ByteString.Char8
                                         hiding ( take )

data Gen = Gen
  { factor :: Int
  , critVal :: Int
  , initVal :: Int
  } deriving Show

values :: Gen -> [Int]
values Gen {..} = filter meetsCriteria $ go initVal
 where
  meetsCriteria v = v `mod` critVal == 0
  go val = let !v = (val * factor) `rem` 2147483647 in v : go v


duel :: Gen -> Gen -> Int -> Int
duel genA genB n = length $ filter id $ take n $ zipWith areEqual
                                                         (values genA)
                                                         (values genB)
  where areEqual = (==) `on` (.&. 0xFFFF)

inputP :: Parser (Int, Int)
inputP = do
  genA <- "Generator A starts with " *> decimal
  endOfLine
  genB <- "Generator B starts with " *> decimal
  pure (genA, genB)

main :: IO ()
main = do
  Right (initA, initB) <- parseOnly inputP <$> B.getContents
  let genA = Gen 16807 1 initA
      genB = Gen 48271 1 initB
      res1 = duel genA genB 40000000
  putStrLn $ "Duel1 count: " ++ show res1

  let genA' = genA { critVal = 4 }
      genB' = genB { critVal = 8 }
      res2  = duel genA' genB' 5000000
  putStrLn $ "Duel2 count: " ++ show res2
