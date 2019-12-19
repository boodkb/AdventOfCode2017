#!/usr/bin/env stack
{- stack --resolver=lts-14.0 script --package=attoparsec,containers,extra -}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main
  ( main
  )
where

import           Data.ByteString.Char8          ( ByteString )
import qualified Data.ByteString.Char8         as B
import           Data.Attoparsec.ByteString.Char8
import           Data.Char
import           Data.Either                    ( either )
import           Data.List

import           Data.Tree
import           Data.Map                       ( Map
                                                , (!)
                                                )
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( fromJust )
import           Data.Foldable


data Program = Program
    { pName :: ByteString
    , pWeight :: Int
    } deriving (Show)


lineParser :: Parser (ByteString, (Int, [ByteString]))
lineParser = do
  progName   <- name
  progWeight <- spaces *> char '(' *> decimal <* char ')'
  names      <- option [] $ do
    spaces *> "->" *> spaces
    name `sepBy` (char ',' *> space)
  pure (progName, (progWeight, names))
 where
  name   = takeWhile1 isAlpha
  spaces = many' space


readInput :: IO (Map ByteString (Int, [ByteString]))
readInput = do
  lines <- B.lines <$> B.getContents
  let ps = sequence $ parseLine <$> lines
  either error (pure . Map.fromList) ps
  where parseLine = parseOnly lineParser


findRoot :: Map ByteString (Int, [ByteString]) -> ByteString
findRoot input = fromJust $ find (`notElem` rs) $ Map.keys lps
 where
  lps = Map.filter (not . null . snd) input
  rs  = foldMap snd lps

buildTree :: Map ByteString (Int, [ByteString]) -> ByteString -> Tree Program
buildTree progMap = unfoldTree makeNode
 where
  makeNode name =
    let (weight, names) = progMap ! name in (Program name weight, names)

weighBranches :: Tree Program -> Tree (Int, Int)
weighBranches = traverseTree
 where
  traverseTree (Node Program {..} children) =
    let children'    = traverseTree <$> children
        branchWeight = pWeight + sum (snd . rootLabel <$> children')
    in  Node (pWeight, branchWeight) children'

findDisbalance :: Tree (Int, Int) -> Int
findDisbalance = fromJust . asum . traverseTree
 where
  traverseTree (Node _ children) =
    let
      childrenBalanced = and $ do
        child <- children
        pure $ areEqual $ snd . rootLabel <$> subForest child

      w = if childrenBalanced
        then findWeight $ rootLabel <$> children
        else Nothing
    in
      Node w (traverseTree <$> children)

areEqual :: Eq a => [a] -> Bool
areEqual []       = True
areEqual (x : xs) = all (== x) xs

-- | (ownWeight, branchWeight)
findWeight :: [(Int, Int)] -> Maybe Int
findWeight ss = case ps of
  ([(xOwn, xBr)], (yOwn, yBr) : _) -> Just $ xOwn + yBr - xBr
  ((xOwn, xBr) : _, [(yOwn, yBr)]) -> Just $ yOwn - (yBr - xBr)
  _ -> Nothing
 where
  minBrW = minimum (snd <$> ss)
  ps     = partition (\e -> snd e == minBrW) ss

main :: IO ()
main = do
  progMap <- readInput
  let rootProg = findRoot progMap
  B.putStrLn $ "Root program is " <> rootProg

  let tree      = weighBranches $ buildTree progMap rootProg
      newWeight = findDisbalance tree
  putStrLn $ "New weight is " ++ show newWeight




