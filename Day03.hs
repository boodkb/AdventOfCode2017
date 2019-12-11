#!/usr/bin/env stack
{- stack --resolver=lts-14.0 script --package=transformers,containers -}
{-# LANGUAGE RecordWildCards #-}
module Main
  ( main
  )
where

import           Prelude                 hiding ( Left
                                                , Right
                                                )

import           Control.Monad.Trans.State
import           Data.Map.Strict

data Position = Position Int Int
  deriving (Show, Eq, Ord)

shiftUp :: Int -> Position -> Position
shiftUp i (Position x y) = Position x (y - i)

shiftDown :: Int -> Position -> Position
shiftDown i (Position x y) = Position x (y + i)

shiftLeft :: Int -> Position -> Position
shiftLeft i (Position x y) = Position (x - i) y

shiftRight :: Int -> Position -> Position
shiftRight i (Position x y) = Position (x + i) y

distance :: Position -> Int
distance (Position x y) = abs x + abs y


-- | Part1

findCorner :: Int -> (Int, Int)
findCorner inp = go 0 1
 where
  go turn val | newVal < inp  = go newTurn newVal
              | newVal == inp = (newTurn, newVal)
              | otherwise     = (turn, val)
   where
    newVal  = newTurn * 8 + val
    newTurn = turn + 1

cornerPosition :: Int -> Position
cornerPosition turn = Position turn turn

data Side = Right | Top | Left | Bottom
  deriving (Show, Enum)

findPosition :: Int -> Int -> Position
findPosition rest turnCount = case toEnum i of
  Right -> shiftUp sidePos initPos
  Top   -> shiftLeft sidePos $ shiftUp sideLength initPos
  Left  -> shiftDown sidePos $ shiftLeft sideLength $ shiftUp sideLength initPos
  Bottom ->
    shiftRight sidePos $ shiftDown sideLength $ shiftLeft sideLength $ shiftUp
      sideLength
      initPos
 where
  (i, sidePos) = rest `divMod` sideLength
  sideLength   = turnCount * 2 + 2
  initPos      = shiftDown 1 $ shiftRight 1 $ cornerPosition turnCount


calcDistance :: Int -> Position
calcDistance i = do
  let (turn, val) = findCorner i
  if val == i then cornerPosition turn else findPosition (i - val) turn


-- | Part2

withLocalState :: (s -> t) -> State t a -> State s a
withLocalState f st = evalState st <$> gets f


type Field = Map Position Int


data CState = CState
    { field :: Field
    , position :: Position
    , side :: Side
    , sideLength :: Int
    } deriving Show

modifyPos :: (Position -> Position) -> CState -> CState
modifyPos f s = s { position = f (position s) }

setSide :: Side -> CState -> CState
setSide newSide s = s { side = newSide }

incSideLength :: State CState ()
incSideLength = modify' $ \s -> s { sideLength = sideLength s + 2 }

moveUp, moveDown, moveLeft, moveRight :: State CState ()
moveUp = modify' $ modifyPos (shiftUp 1) . setSide Right
moveDown = modify' $ modifyPos (shiftDown 1) . setSide Left
moveLeft = modify' $ modifyPos (shiftLeft 1) . setSide Top
moveRight = modify' $ modifyPos (shiftRight 1) . setSide Bottom


moveNext :: State CState ()
moveNext = do
  CState {..} <- get
  let Position x y = position
      sidePos coord = coord + (sideLength - 1) `div` 2

  case side of
    Bottom | sidePos x < sideLength -> moveRight
           | otherwise              -> incSideLength >> moveUp

    Right | sidePos y > 0 -> moveUp
          | otherwise     -> moveLeft

    Top | sidePos x > 0 -> moveLeft
        | otherwise     -> moveDown

    Left | sidePos y < sideLength - 1 -> moveDown
         | otherwise                  -> moveRight


getValue :: State CState Int
getValue = do
  CState {..} <- get
  pure $ findWithDefault 0 position field

putValue :: Int -> State CState ()
putValue newValue = do
  s@CState {..} <- get
  put s { field = insert position newValue field }


calculateNewValue :: State CState Int
calculateNewValue = do
  newVal <- withLocalState id $ sum <$> sequence
    [ moveRight >> getValue
    , moveUp >> getValue
    , moveLeft >> getValue
    , moveLeft >> getValue
    , moveDown >> getValue
    , moveDown >> getValue
    , moveRight >> getValue
    , moveRight >> getValue
    ]
  putValue newVal
  pure newVal


findFirst :: Int -> State CState Int
findFirst i = loop
 where
  loop = do
    n <- moveNext >> calculateNewValue
    if n > i then pure n else loop


main :: IO ()
main = do
  input <- readLn
  let position = calcDistance input
  putStrLn $ "Position = " ++ show position
  putStrLn $ "Distance = " ++ show (distance position)

  let initState = CState { field      = fromList [(Position 0 0, 1)]
                         , position   = Position 0 0
                         , side       = Bottom
                         , sideLength = 1
                         }
      number = evalState (findFirst input) initState
  putStrLn $ "Number = " ++ show number


