{-# OPTIONS -Wall #-}

module Sudoku where

import Formula
import Puzzle
import Lib

import Data.Maybe
import Data.List

data Sudoku = Sudoku Int Int (Matrix Int)

instance Puzzle Sudoku where
  maxVariable (Sudoku rsize csize _) = (rsize * csize) ^ (3 :: Int)
  
  varMatrix (Sudoku rsize csize _) = divide len $ divide len [1..len * len * len]
    where
      len = rsize * csize
  
  constraint sudoku =
    allAnd [ entryConstraint varMat
           , rowConstraint varMat
           , colConstraint varMat
           , boxConstraint sudoku varMat
           , inputConstraint sudoku 
           ]
    where
      varMat = map (map (map Var)) $ varMatrix sudoku
  
  showPuzzle (Sudoku rsize csize _) vns = do
    let vns' = divide len $ divide len vns
    mapM_ (putStrLn . concat . intersperse " " . map show . map succ . map fromJust . map (findIndex (> 0))) vns' 
    where
      len = rsize * csize
  
  parsePuzzle puzzleStr = Sudoku rsize csize matrix
    where
      [rsize, csize] = map read . words . head $ puzzleStr
      matrix = map (map read) . map words . filter (not . null) . tail $ puzzleStr
  
  encodeType _ = None

entryConstraint :: Matrix [Formula] -> Formula     
entryConstraint = allAnd . map allAnd . map (map allOr)

rowConstraint :: Matrix [Formula] -> Formula
rowConstraint varmat = 
  allAnd [ allAnd 
           [ allAnd 
             [ Or (Not ent1) (Not ent2)  
             | ent1 <- var, ent2 <- var, ent1 /= ent2
             ]
           | var <- transpose row
           ]
         | row <- varmat
         ]

colConstraint :: Matrix [Formula] -> Formula
colConstraint varmat = 
  allAnd 
  [ allAnd 
    [ allAnd 
      [ Or (Not ent1) (Not ent2)
      | ent1 <- var, ent2 <- var, ent1 /= ent2
      ]
    | var <- transpose col 
    ]
  | col <- transpose varmat
  ]                

boxConstraint :: Sudoku -> Matrix [Formula] -> Formula
boxConstraint (Sudoku rsize csize _) varmat = 
    allAnd 
  [ allAnd 
    [ allAnd 
      [ Or (Not ent1) (Not ent2)
      | ent1 <- var, ent2 <- var, ent1 /= ent2
      ]
    | var <- transpose box 
    ]
  | box <- map concat . concat . map (divide rsize) . transpose . map (divide csize) $ varmat 
  ]      
  
inputConstraint :: Sudoku -> Formula
inputConstraint (Sudoku _ _ mat) = allAnd vars  
  where
    len = length mat
    bools = concat . map (foo len) . concat $ mat
    vars = map Var . map fst . filter snd . zip [1..] $ bools
                      
foo :: Int -> Int -> [Bool]
foo len n = 
  if n == 0                      
    then replicate len False 
    else replicate (n - 1) False ++ True : replicate (len - n) False 