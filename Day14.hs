#!/usr/bin/env stack
{- stack --resolver=lts-14.0 script --package=vector,split,mtl,transformers,text,bytestring,bits-bytestring,containers -}
{-# LANGUAGE TupleSections #-}
module Main
  ( main
  )
where

import qualified Data.Text                     as T
import           Data.Bits
import           Data.Bits.ByteString           ( )
import qualified Data.ByteString               as B
import qualified Data.Vector                   as V
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Control.Monad.Trans.State.Strict

import           KnotHash

type Square = (Int, Int)

neighbours :: Square -> [Square]
neighbours (r, c) = [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)]

processRegion :: Square -> State (Set Square) ()
processRegion sq = do
  modify' $ Set.delete sq
  sqs <- get
  mapM_ processRegion $ filter (`Set.member` sqs) $ neighbours sq

countRegions :: State (Set Square) Int
countRegions = go 0
 where
  go n = do
    sqs <- get
    if Set.null sqs
      then pure n
      else processRegion (Set.elemAt 0 sqs) >> go (n + 1)


usedSquares :: B.ByteString -> [Int]
usedSquares bs = (maxIndex -) <$> filter (testBit bs) [0 .. maxIndex]
  where maxIndex = bitSize bs - 1


main :: IO ()
main = do
  input <- getContents
  let hashInput i = T.pack $ input <> "-" <> show i
      hashes    = V.fromList $ knotHash . hashInput <$> [0 .. 127]
      usedInRow = B.foldl' (\acc bt -> acc + popCount bt) 0
      used      = V.sum $ usedInRow <$> hashes
  putStrLn $ "Used: " ++ show used

  let squares = V.ifoldl' f mempty hashes
      f acc row hash =
        Set.union acc $ Set.fromList $ (row, ) <$> usedSquares hash
      regions = evalState countRegions squares
  putStrLn $ "Regions: " ++ show regions
