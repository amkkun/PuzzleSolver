{-# OPTIONS -Wall #-}

module Main where

import Puzzle
import Sudoku
import Lib


import System.Environment
import System.Process

import Control.Applicative ((<$>))
import Control.Monad

data PuzzleType = SudokuType


main :: IO ()
main = do
  [filepath] <- getArgs 
  puzzleStr <- lines <$> readFile filepath
  assign puzzleStr
  
assign :: [String] -> IO ()
assign puzzleStr = 
  forM_ puzzleStr $ \str ->
    solve $ Sudoku 3 3 $ divide 9 $ map read $ divide 1 str 
  
solve :: Sudoku -> IO ()
solve puzzle = do
  writeDIMACS puzzle
  (exitcode, stdout, stderr) <- readProcessWithExitCode "minisat" ["puzzle.dimacs", "answer.dimacs"] ""
  readDIMACS puzzle
  putStrLn "--"