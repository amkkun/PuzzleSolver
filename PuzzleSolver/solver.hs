module Main where

import IllustLogic
import Puzzle

import System.Environment
import System.Process

import Control.Applicative ((<$>))

                
main :: IO ()
main = do
  (puzzleTypeS:filepath:_) <- getArgs
  puzzleS <- lines <$> readFile filepath
  let puzzleType = readPuzzleType puzzleTypeS
      puzzle = parsePuzzle puzzleType puzzleS :: IllustLogic
  writeDIMACS puzzle
  (exitcode, stdout, stderr) <- readProcessWithExitCode "minisat" ["puzzle.dimacs", "answer.dimacs"] ""
  readDIMACS puzzle
  return ()