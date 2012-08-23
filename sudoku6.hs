{-# OPTIONS -Wall #-}

module Main where

import Data.Map (Map)
import qualified Data.Map as M

type Pos = (Int, Int)
type CellPos = (Pos, Pos)

type Value = Int
type Matrix = [[Value]]
type PosVal = [(CellPos, [Value])] -- Map CellPos [Value]
type ValPos = [(Value, [CellPos])] -- Map Value [CellPos]
  
type Sudoku = (Int, Int, Matrix)
              
toValPos :: PosVal -> ValPos

toPosVal :: ValPos -> PosVal


elimVal :: PosVal -> PosVal

elimPos :: ValPos -> ValPos

eliminate :: PosVal -> PosVal
eliminate = toPosVal . elimPos . toValPos . elimVal


rows :: PosVal -> [PosVal]

cols :: PosVal -> [PosVal]

boxs :: PosVal -> [PosVal]




mainSolve :: PosVal -> [PosVal]
mainSolve pv
  | isCompleted pv = return pv
  | otherwise = mainSolve $ eliminate pv
  

solve :: Sudoku -> [Matrix]
solve = map toMatrix . mainSolve . analyze

analyze :: Sudoku -> PosVal

toMatrix :: PosVal -> Matrix


display :: Matrix -> IO ()

getSudoku :: IO Sudoku
getSudoku = do
  (rowsize:colsize:_) <- map read . words <$> getLine
  let boardsize = rowsize * colsize
  matrix <- forM [1..boardsize] (\_ -> map read . words <$> getLine)
  return (rowsize, colsize, matrix)
  
main :: IO ()
main = do
  sudoku <- getSudoku
  let answers = solve sudoku
  mapM_ display answers