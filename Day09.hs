#!/usr/bin/env stack
{- stack --resolver=lts-14.0 script package=megaparsec,text,containers,transformers,mtl -}
{-# LANGUAGE OverloadedStrings, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ConstraintKinds #-}
module Main
  ( main
  )
where

import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Data.Functor
import           Data.Void
import           Control.Monad.Writer.Strict
import           Data.Text
import qualified Data.Text.IO                  as T

type StreamM m = (MonadParsec Void Text m, MonadWriter (Sum Int, Sum Int) m)

ignoreP :: StreamM m => m ()
ignoreP = void $ char '!' *> anySingle

garbageP :: StreamM m => m ()
garbageP = char '<' *> skipMany gc <* char '>'
 where
  gc          = choice [ignoreP, garbageChar]
  garbageChar = anySingleBut '>' >> tell (Sum 0, Sum 1)

groupP :: StreamM m => Int -> m ()
groupP l = do
  char '{' *> many inner *> char '}'
  tell (Sum l, Sum 0)
 where
  inner = choice [ignoreP, garbageP, groupP (l + 1), other]
  other = void $ noneOf ['{', '}']

parser :: StreamM m => m ()
parser = choice [groupP 1, garbageP]

main :: IO ()
main = do
  Right (Sum score, Sum garbage) <- parseStream <$> T.getContents
  putStrLn $ "The total score: " ++ show score
  putStrLn $ "Characters within the garbage: " ++ show garbage
  where parseStream = runParser (execWriterT parser) mempty

