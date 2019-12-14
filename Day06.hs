#!/usr/bin/env stack
{- stack --resolver=lts-14.0 script --package=vector,containers -}
{-# LANGUAGE TypeFamilies #-}
module Main
  ( main
  )
where

import           Data.Vector.Unboxed           as V
                                                ( fromList
                                                , Vector
                                                , thaw
                                                , freeze
                                                , maxIndex
                                                )
import           Data.Vector.Unboxed.Mutable    ( IOVector )
import qualified Data.Vector.Unboxed.Mutable   as V

import           Data.Map                       ( Map )
import qualified Data.Map.Strict               as Map
import           Control.Monad


readVector :: IO (Vector Int)
readVector = do
  ls <- words <$> getContents
  pure $ V.fromList $ read <$> ls



redistribute :: IOVector Int -> Int -> IO ()
redistribute vec pos = do
  bl <- V.read vec pos
  V.write vec pos 0
  loop bl (next pos)
 where
  next i = (i + 1) `mod` V.length vec
  loop blocks pos = do
    V.modify vec (+ 1) pos
    when (blocks - 1 > 0) $ loop (blocks - 1) (next pos)



run :: Vector Int -> IO (Int, Int)
run vector = V.thaw vector >>= loop mempty (maxIndex vector) 0
 where
  loop prevStates ix step vec = do
    redistribute vec ix
    curState <- V.freeze vec
    case Map.lookup curState prevStates of
      Just prevStep -> pure (step + 1, step - prevStep)
      Nothing       -> loop (Map.insert curState step prevStates)
                            (maxIndex curState)
                            (step + 1)
                            vec


main :: IO ()
main = do
  vec          <- readVector
  (step, size) <- run vec
  putStrLn $ "Step = " ++ show step
  putStrLn $ "Size = " ++ show size

