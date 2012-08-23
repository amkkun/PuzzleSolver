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
elimVal = elimSub boxs . elimSub cols . elimSub rows

elimSub :: (PosVal -> [PosVal]) -> PosVal -> PosVal
elimSub f = concat . reduce <$> f

elimPos :: ValPos -> ValPos

eliminate :: PosVal -> PosVal
eliminate = toPosVal . elimPos . toValPos . elimVal



fixed :: [[a]] -> [a]
fixed = concat . filter single

single :: [a] -> Bool
single = (==) 1 . length

delete :: [a] -> [a] -> [a]
delete xs ys
  | single xs = xs
  | otherwise = filter (`notElem` ys) xs

reduce :: PosVal -> PosVal
reduce pv = map (\(pos, vs) -> (pos, delete vs fixedVals)) pv
  where
    fixedVals = fixed $ map snd pv



rows :: PosVal -> [PosVal]
rows = groupBy (\(p1, _) (p2, _) -> isSameRow p1 p2)
  
cols :: PosVal -> [PosVal]
cols = groupBy (\(p1, _) (p2, _) -> isSameCol p1 p2)

boxs :: PosVal -> [PosVal]
boxs = groupBy (\(p1, _) (p2, _) -> isSameBox p1 p2)

-- groupMapBy :: (Ord k) => (k -> k -> Bool) -> Map k a -> [Map k a]


-- intSqrt :: Num a => a -> a
-- intSqrt n = intSqrt' n 1
--   where
--     intSqrt' n m 
--       | n < m * m = m - 1
--       | otherwise = intSqrt' n (m + 1)

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

toMatrix :: PosVal -> Matrix


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