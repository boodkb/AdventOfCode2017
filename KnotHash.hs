{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
module KnotHash
  ( knotHash
  , single
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

import           Control.Monad.State.Strict

import           Data.Text                      ( Text )
import qualified Data.Text                     as T

import qualified Data.ByteString               as B


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


single :: [Int] -> Int
single lengths = (nums ! 0) * (nums ! 1)
 where
  nums = Vec.modify (\v -> evalStateT (knotRound v lengths) (0, 0)) numbers


knotHash :: Text -> B.ByteString
knotHash input =
  let sparseHash = Vec.toList $ Vec.modify
        (\v -> evalStateT (replicateM_ 64 (knotRound v lengths)) (0, 0))
        numbers
      denseHash = foldl1 xor <$> chunksOf 16 sparseHash
  in  B.pack $ fromIntegral <$> denseHash
  where lengths = (ord <$> T.unpack input) ++ [17, 31, 73, 47, 23]
