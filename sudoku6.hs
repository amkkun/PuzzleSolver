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
toValPos pv = do
  n <- values
  return (n, map fst $ filter (\(_, vs) -> elem n vs) pv) 

toPosVal :: ValPos -> PosVal



elimVal :: PosVal -> PosVal
elimVal = elimValSub boxsPV . elimValSub colsPV . elimValSub rowsPV

elimValSub :: (PosVal -> [PosVal]) -> PosVal -> PosVal
elimValSub f = concat . map reduce . f


fixed :: [[a]] -> [a]
fixed = concat . filter single

isFixedPV :: (CellPos, [Value]) -> Bool
isFixedPV = single . snd

single :: [a] -> Bool
single = (==) 1 . length

delete :: [a] -> [a] -> [a]
delete xs ys
  | single xs = xs
  | otherwise = filter (`notElem` ys) xs

reduce :: PosVal -> PosVal
reduce pv = map (\(pos, vs) -> (pos, delete vs fixedVals)) pv
  where
    fixedVals = concat $ filter isFixedPV pv >>= return . snd 



rowsPV :: PosVal -> [PosVal]
rowsPV = groupBy (\(p1, _) (p2, _) -> isSameRow p1 p2)
  
colsPV :: PosVal -> [PosVal]
colsPV = groupBy (\(p1, _) (p2, _) -> isSameCol p1 p2)

boxsPV :: PosVal -> [PosVal]
boxsPV = groupBy (\(p1, _) (p2, _) -> isSameBox p1 p2)


rowsVP :: ValPos -> [ValPos]
rowsVP vp = [[(v, ps) | ps <- pss] | (v, pss) <- vps]
  where
    vps = map (\(v, ps) -> (v, groupBy isSameRow ps)) vp

colsVP :: ValPos -> [ValPos]
rowsVP vp = [[(v, ps) | ps <- pss] | (v, pss) <- vps]
  where
    vps = map (\(v, ps) -> (v, groupBy isSameCol ps)) vp

boxsVP :: ValPos -> [ValPos]
rowsVP vp = [[(v, ps) | ps <- pss] | (v, pss) <- vps]
  where
    vps = map (\(v, ps) -> (v, groupBy isSameBox ps)) vp

backVPsmall :: ValPos -> (Value, [CellPos])
backVPsmall vp = 

backVP :: [ValPos] -> ValPos  
backVP = map backVPsmall

elimPos :: ValPos -> ValPos
elimPos = backVP . map elimPosTwo . boxsVP . backVP . map elimPosTwo . colsVP . backVP . map elimPosTwo . rowsVP

eliminate :: PosVal -> PosVal
eliminate = toPosVal . elimPos . toValPos . elimVal



isSameRow :: CellPos -> CellPos -> Bool
isSameRow p1 p2 = fst (fst p1) == fst (fst p2) &&
                  fst (snd p1) == fst (snd p2)

isSameCol :: CellPos -> CellPos -> Bool
isSameCol p1 p2 = snd (fst p1) == snd (fst p2) &&
                  snd (snd p1) == snd (snd p2)

isSameBox :: CellPos -> CellPos -> Bool
isSameBox p1 p2 = fst p1 == fst p2



mainSolve :: PosVal -> [PosVal]
mainSolve pv
  | isCompleted pv = return pv
  | otherwise = mainSolve $ eliminate pv
  

solve :: Sudoku -> [Matrix]
solve = map toMatrix . mainSolve . analyze

analyze :: Sudoku -> PosVal
analyze (row, col, matrix) = map (divide row) matrix

toMatrix :: PosVal -> Matrix

divide :: Int -> [a] -> [[a]]
divide _ [] = []
divide n xs = take n xs : divide n (drop n xs)

display :: Matrix -> IO ()
display = mapM_ (putStrLn . concat . intersperse " " . map show)

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