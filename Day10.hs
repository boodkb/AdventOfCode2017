#!/usr/bin/env stack
{- stack --resolver=lts-14.0 script --package=vector,split,attoparsec,mtl,transformers,hexstring,text,bytestring -}
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
module Main
  ( main
  )
where

import           Data.Vector.Unboxed            ( Vector
                                                , (!)
                                                )
import qualified Data.Vector.Unboxed           as Vec
import qualified Data.Vector.Unboxed.Mutable   as MVec
import           Data.Foldable
import           Data.List.Split                ( chunksOf )
import           Data.Char
import           Data.Bits
import           Data.HexString

import           Control.Monad.Reader
import           Control.Monad.State.Strict

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T

import qualified Data.ByteString               as B
import           Data.Attoparsec.Text    hiding ( takeWhile )


numbers :: Vector Int
numbers = Vec.fromList [0 .. 255]

swapIndexes :: Int -> Int -> [(Int, Int)]
swapIndexes start len = takeWhile ls $ makePair <$> [0 ..]
 where
  ls (x, y) = x < y
  makePair i = (start + i, start + len - i - 1)


knotRound vec lengths = for_ lengths $ \len -> do
  (pos, skipSize) <- get
  lift $ for_ (swapIndexes pos len) $ \(i, j) ->
    MVec.swap vec (vecIx i) (vecIx j)
  put (pos + len + skipSize, skipSize + 1)
  where vecIx i = i `rem` MVec.length vec


single :: Text -> IO Int
single input = do
  let Right lengths = parseOnly lengthsP input
      nums = Vec.modify (\v -> evalStateT (knotRound v lengths) (0, 0)) numbers
  pure $ (nums ! 0) * (nums ! 1)
  where lengthsP = decimal `sepBy` char ','


knotHash :: Text -> IO Text
knotHash input = do
  let sparseHash = Vec.toList $ Vec.modify
        (\v -> evalStateT (replicateM_ 64 (knotRound v lengths)) (0, 0))
        numbers
      denseHash = foldl1 xor <$> chunksOf 16 sparseHash
  pure $ toText $ fromBytes $ B.pack $ fromIntegral <$> denseHash
  where lengths = (ord <$> T.unpack input) ++ [17, 31, 73, 47, 23]


main :: IO ()
main = do
  input <- T.strip <$> T.getContents
  i     <- single input
  print i
  hash <- knotHash input
  T.putStrLn hash

