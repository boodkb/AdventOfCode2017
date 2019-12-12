#!/usr/bin/env stack
{- stack --resolver=lts-14.0 script --package=vector -}
{-# LANGUAGE TypeFamilies #-}
module Main
  ( main
  )
where


import           Data.Vector.Unboxed           as V
                                                ( fromList
                                                , Vector
                                                , thaw
                                                )
import           Data.Vector.Unboxed.Mutable    ( IOVector )
import qualified Data.Vector.Unboxed.Mutable   as V

readVector :: IO (Vector Int)
readVector = do
  ls <- lines <$> getContents
  pure $ V.fromList $ read <$> ls


run :: (Int -> Int) -> IOVector Int -> IO Int
run updVal vec = loop 0 0
 where
  loop pos step = do
    val <- V.read vec pos
    let newPos  = pos + val
        newStep = step + 1
    V.modify vec updVal pos
    if newPos >= V.length vec then pure newStep else loop newPos newStep

main :: IO ()
main = do
  vec <- readVector
  V.thaw vec >>= run (+ 1) >>= print
  let updVal val = if val > 2 then val - 1 else val + 1
  V.thaw vec >>= run updVal >>= print
