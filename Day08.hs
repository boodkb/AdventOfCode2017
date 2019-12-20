#!/usr/bin/env stack
{- stack --resolver=lts-14.0 script package=attoparsec,bytestring,containers,transformers,mtl -}
{-# LANGUAGE OverloadedStrings, FlexibleContexts,
   ConstraintKinds #-}
module Main
  ( main
  )
where

import           Data.Char
import qualified Data.ByteString.Char8         as B
import           Data.Attoparsec.ByteString.Char8
import           Control.Monad
import           Data.Either
import           Data.Functor
import           Data.Semigroup

import           Data.Map                       ( Map )
import qualified Data.Map.Strict               as Map
import           Control.Monad.State.Strict
import           Control.Monad.Writer.Strict

type Register = B.ByteString
type Rel = (Int -> Int -> Bool)
type Op = (Int -> Int -> Int)

data Condition
  = Condition Register Rel Int

data Instruction
  = Instruction Register Op Int Condition


intP :: Parser Int
intP = signed decimal

regP :: Parser Register
regP = takeWhile1 isAlpha

relP :: Parser Rel
relP = choice
  [ ">=" $> (>=)
  , ">" $> (>)
  , "<=" $> (<=)
  , "<" $> (<)
  , "==" $> (==)
  , "!=" $> (/=)
  ]

condP :: Parser Condition
condP = do
  reg <- "if" *> skipSpace *> regP
  rel <- skipSpace *> relP
  val <- skipSpace *> intP
  pure $ Condition reg rel val

opP :: Parser Op
opP = choice ["inc" $> (+), "dec" $> (-)]

instP :: Parser Instruction
instP = do
  reg  <- regP
  op   <- skipSpace *> opP
  val  <- skipSpace *> intP
  cond <- skipSpace *> condP
  pure $ Instruction reg op val cond



type Registers = Map Register Int
type CpuM m = (MonadState Registers m, MonadWriter (Max Int) m)

getValue :: MonadState Registers m => Register -> m Int
getValue reg = gets $ Map.findWithDefault 0 reg

insertValue :: MonadState Registers m => Register -> Int -> m ()
insertValue reg val = modify' $ Map.insert reg val

isMet :: MonadState Registers m => Condition -> m Bool
isMet (Condition reg rel val) = do
  regVal <- getValue reg
  pure $ rel regVal val


updateRegValue :: CpuM m => Register -> (Int -> Int) -> m ()
updateRegValue reg f = do
  val <- getValue reg
  let newVal = f val
  tell $ Max newVal
  insertValue reg newVal

interpret :: CpuM m => [Instruction] -> m ()
interpret code = forM_ code $ \(Instruction reg op val cond) -> do
  condMet <- isMet cond
  when condMet $ updateRegValue reg $ flip op val


main :: IO ()
main = do
  ls <- B.lines <$> B.getContents
  let parsed             = sequence $ parseOnly instP <$> ls
      code               = fromRight [] parsed
      (Max maxVal, regs) = run $ interpret code
  putStrLn $ "The largest value after the process is " ++ show (maximum regs)
  putStrLn $ "The highest value during the process is " ++ show maxVal
  where run = flip runState mempty . execWriterT
