#!/usr/bin/env stack
{- stack --resolver=lts-14.0 script --package=vector,split,attoparsec,mtl,transformers,hexstring,text,bytestring -}
module Main
  ( main
  )
where


import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Data.Attoparsec.Text    hiding ( takeWhile )
import           Data.HexString

import           KnotHash


main :: IO ()
main = do
  input <- T.strip <$> T.getContents

  let lengthsP      = decimal `sepBy` char ','
      Right lengths = parseOnly lengthsP input
  print $ single lengths

  T.putStrLn $ toText $ fromBytes $ knotHash input

