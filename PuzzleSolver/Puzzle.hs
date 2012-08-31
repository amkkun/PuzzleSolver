module Puzzle where

import Formula

import qualified Data.ByteString.Lazy.Char8 as C
import Control.Applicative ((<$>))

data EncodeType = Tseitin | Normal | None

type Matrix a = [[a]]



class Puzzle a where
  maxVariable :: a -> VarNum  
  
  varMatrix :: a -> Matrix [VarNum]

  constraint :: a -> Formula
  
  showPuzzle :: a -> [VarNum] -> IO ()
  
  parsePuzzle :: [String] -> a
  
  encodeType :: a -> EncodeType
  
  writeDIMACS :: a -> IO ()
  writeDIMACS puzzle = case encodeType puzzle of
    Tseitin -> C.writeFile "puzzle.dimacs" . dimacsTseitin $ constraint puzzle
    Normal -> C.writeFile "puzzle.dimacs" . dimacsNormal $ constraint puzzle
    None -> C.writeFile "puzzle.dimacs" . toDimacsString . toCNF . elimUseless $ constraint puzzle
      
  readDIMACS :: a -> IO ()
  readDIMACS puzzle = do
    ans <- lines <$> readFile "answer.dimacs"
    if head ans == "SAT"
      then showPuzzle puzzle . take (maxVariable puzzle) . map read . words $ ans !! 1
      else putStrLn $ head ans