{-# OPTIONS -Wall #-}

module Sudoku where

import Formula
import Puzzle
import Lib

data Sudoku = Sudoku Int Int (Matrix Int)

instance Puzzle Sudoku where
  varMatrix (Sudoku rsize csize _) = divide len $ divide len [1..len * len * len]
    where
      len = rsize * csize
  
  constraint sudoku =
    allAnd [ entryConstraint varMat
           , rowConstraint varMat
           , colConstraint varMat
           , boxConstraint varMat
           , 
           ]
    where
      varMat = varMatrix sudoku
      
entryConstraint :: [Matrix VarNum] -> Formula     
entryConstraint =  . map allOr 