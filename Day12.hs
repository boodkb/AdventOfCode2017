#!/usr/bin/env stack
{- stack --resolver=lts-14.0 script --package=attoparsec,bytestring,containers -}
{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Main
  ( main
  )
where

import qualified Data.ByteString               as B
import           Data.Attoparsec.ByteString.Char8
import           Data.Graph

graphP :: Parser [((), Int, [Int])]
graphP = lineP `sepBy` endOfLine
  where lineP = ((), , ) <$> decimal <*> (" <-> " *> decimal `sepBy` ", ")

main :: IO ()
main = do
  Right edges <- parseOnly graphP <$> B.getContents
  let (graph, _) = graphFromEdges' edges
      c          = length $ reachable graph 0
      total      = length $ components graph
  putStrLn $ "Size: " ++ show c
  putStrLn $ "Total: " ++ show total
