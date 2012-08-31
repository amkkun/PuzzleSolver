{-# OPTIONS -Wall #-}

module Main where

import Puzzle
import IllustLogic

import System.Environment
import System.Process

import Control.Applicative ((<$>))


data PuzzleType = IllustLogicType


main :: IO ()
main = do
  (puzzleType, filepath) <- getArgs >>= checkArgs
  puzzleStr <- lines <$> readFile filepath
  assign puzzleType puzzleStr
  
checkArgs :: [String] -> IO (PuzzleType, FilePath)
checkArgs [puzzleType, path] = 
  return (readPuzzleType puzzleType, path)
checkArgs _ = do
  error ""

readPuzzleType :: String -> PuzzleType
readPuzzleType s
--  | s == "sudoku" = SudokuType
  | s == "illustlogic" = IllustLogicType
  | otherwise = error "sudoku or illustlogic"


assign :: PuzzleType -> [String] -> IO ()
assign puzzleType puzzleStr = case puzzleType of
  IllustLogicType -> solve $ (parsePuzzle puzzleStr :: IllustLogic)
  

solve :: Puzzle p => p -> IO ()
solve puzzle = do
  writeDIMACS puzzle
  (exitcode, stdout, stderr) <- readProcessWithExitCode "minisat" ["puzzle.dimacs", "answer.dimacs"] ""
  readDIMACS puzzle
  