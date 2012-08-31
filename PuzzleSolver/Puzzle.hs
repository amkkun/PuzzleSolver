module Puzzle where

import Formula

import qualified Data.ByteString.Lazy.Char8 as C
import Control.Applicative ((<$>))

type Matrix a = [[a]]


class Puzzle a where
  maxVariable :: a -> VarNum  
  
  varMatrix :: a -> Matrix [VarNum]

  constraint :: a -> Formula
  
  showPuzzle :: a -> [VarNum] -> IO ()
  
  parsePuzzle :: [String] -> a
  
  writeDIMACS :: a -> IO ()
  writeDIMACS = C.writeFile "puzzle.dimacs" . dimacsTseitin . constraint
  
  readDIMACS :: a -> IO ()
  readDIMACS puzzle = do
    ans <- lines <$> readFile "answer.dimacs"
    if head ans == "SAT"
      then showPuzzle puzzle . take (maxVariable puzzle) . map read . words $ ans !! 1
      else putStrLn $ head ans