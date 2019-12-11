#!/usr/bin/env stack
{- stack --resolver=lts-14.0 script -}
module Main
  ( main
  )
where

import           Data.List

type Passphrase = [String]

isValid1 :: Passphrase -> Bool
isValid1 ps = length ps == length (nub ps)

isValid2 :: Passphrase -> Bool
isValid2 ps = all checkOne $ take len . dropped <$> [0 .. len - 1]
 where
  dropped i = drop i cps
  cps = cycle ps
  len = length ps
  checkOne (x : xs) = all (`notElem` permutations x) xs

main :: IO ()
main = do
  passphrases <- fmap words . lines <$> getContents
  print $ length $ filter isValid1 passphrases
  print $ length $ filter isValid2 passphrases
