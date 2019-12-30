#!/usr/bin/env stack
{- stack --resolver=lts-14.0 script --package=attoparsec,bytestring -}
{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  )
where

import qualified Data.ByteString               as B
import           Data.Attoparsec.ByteString.Char8

data Scanner = Scanner
    { layer :: Int
    , range :: Int
    } deriving Show

scannerSeverity :: Scanner -> Int
scannerSeverity sc = layer sc * range sc

isOnTopAfter :: Int -> Scanner -> Bool
isOnTopAfter sec sc = sec `mod` ((range sc - 1) * 2) == 0

inputP :: Parser [Scanner]
inputP = scannerP `sepBy` endOfLine
  where scannerP = Scanner <$> decimal <*> (": " *> decimal)

severity :: [Scanner] -> Int
severity scs = sum $ scannerSeverity <$> filter f scs
  where f sc = isOnTopAfter (layer sc) sc

delay :: [Scanner] -> Int
delay scs = go [0 ..]
 where
  f i sc = isOnTopAfter (layer sc + i) sc
  go (i : is) = if any (f i) scs then go is else i

main :: IO ()
main = do
  Right scanners <- parseOnly inputP <$> B.getContents
  let sev = severity scanners
  putStrLn $ "Severity: " ++ show sev

  let del = delay scanners
  putStrLn $ "Delay: " ++ show del


