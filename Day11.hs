#!/usr/bin/env stack
{- stack --resolver=lts-14.0 script package=attoparsec,bytestring -}
{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  )
where

import           Data.Semigroup
import           Data.Functor
import qualified Data.ByteString               as B
import           Data.Attoparsec.ByteString.Char8

data Steps = Steps
  { ns :: Int
  , ew :: Int
  } deriving Show

instance Semigroup Steps where
  Steps x1 y1 <> Steps x2 y2 = Steps (x1 + x2) (y1 + y2)

instance Monoid Steps where
  mempty = Steps 0 0

distance :: Steps -> Int
distance (Steps x y) | ax <= ay = max ax ay
                     | ax > ay  = ay + (ax - ay) `div` 2
 where
  ax = abs x
  ay = abs y

stepP :: Parser Steps
stepP = choice
  [ "ne" $> Steps 1 1
  , "nw" $> Steps 1 (-1)
  , "n" $> Steps 2 0
  , "se" $> Steps (-1) 1
  , "sw" $> Steps (-1) (-1)
  , "s" $> Steps (-2) 0
  ]


fun :: (Steps, Max Int) -> Steps -> (Steps, Max Int)
fun (pos, md) step = (newPos, md <> Max newDist)
 where
  newDist = distance newPos
  newPos  = pos <> step

main :: IO ()
main = do
  Right steps <- parseOnly (stepP `sepBy` char ',') <$> B.getContents
  let (path, Max maxDist) = foldl fun mempty steps
  putStrLn $ "Distance: " ++ show (distance path)
  putStrLn $ "The furthest distance: " ++ show maxDist

