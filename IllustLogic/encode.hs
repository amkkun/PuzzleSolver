{-# OPTIONS -Wall #-}

module Main where

import Expr
import Data.List

data Cell = Blank | Fill deriving (Show, Eq)
data Grid = Grid Int Int Cell deriving (Show, Eq)

-- (n - sum l - (length l - 1)) 個のものを (length l + 1) つの組にわける



-- sum (distribute n k) == n
-- distribute 2 3 = [[2,0,0],[1,1,0],[1,0,1],[0,2,0],[0,1,1],[0,0,2]]
distribute :: Int -> Int -> [[Int]] -- n wo k ko no kumi ni wakeru. 
distribute n size 
  | size <= 0 = []
  | size == 1 = [[n]]
  | otherwise = [m : d | m <- [0..n], d <- distribute (n - m) (size - 1)]

-- length l + 1 == length d
mutually :: [a] -> [a] -> [a]
mutually [] _ = []
mutually (x:_) [] = [x]
mutually (x:xs) (y:ys) = x : y : mutually xs ys

each :: Int -> [[a]] -> [[a]]
each _ [] = []
each n xss = concat (take n xss) : each n (drop n xss)

cells :: [Int] -> [Int] -> [Cell]
cells fs bs = concat $ mutually bs' (each 2 $ intersperse [Blank] fs')
  where
    bs' = map (`replicate` Blank) bs
    fs' = map (`replicate` Fill) fs
    
constraint :: Int -> [Int] -> [[Cell]]
constraint size fs
  | b < 0 = error ("constraint: " ++ show fs)
  | otherwise = distribute b (length fs + 1) >>= return . cells fs     
  where
    b = size - sum fs - (length fs - 1) -- 列の長さ - 連続して塗りつぶす数の合計 - 間の空白の数 = 残りの空白の数
    

line :: Int -> [[Cell]] -> [[Grid]] -- line number -> cellss -> gridss
line n = map (\c -> [Grid n i (c !! pred i) | i <- [1..length c]])

grid :: [[[Cell]]] -> [[[Grid]]]
grid c = [line i (c !! pred i) | i <- [1..length c]]

condition :: Int -> [[Int]] -> [[[Grid]]]
condition size = grid . map (constraint size) 

transpose :: Grid -> Grid
transpose (Grid r c cell) = Grid c r cell

transposeAll :: [[[Grid]]] -> [[[Grid]]]
transposeAll = map (map (map Main.transpose))

toExpr :: [[[Grid]]] -> Expr Grid
toExpr = allAnd . map toCnf . map allOr . map (map allAnd) . map (map (map expr))
  where
    expr g = Var g


--

--        1 
--       21113
--   2 1 **..*
-- 1 1 1 *.*.*
--   1 2 .*.**

rsize :: Info -> Int
rsize (Info r _ _ _) = r

csize :: Info -> Int 
csize (Info _ _ c _) = c

toNum :: Expr Grid -> Int -- var -> num
toNum (Var (Grid r c w)) = if w == Fill 
                           then (r - 1) * rsize info + c
                           else negate $ (r - 1) * rsize info + c
toNum (Not e) = negate $ toNum e
toNum _ = error "toNum: It is NOT variable."


encode :: Expr Grid -> String -- CNF 
encode (And e1 e2) = encode e1 ++ " 0\n" ++ encode e2
encode (Or e1 e2) = encode e1 ++ " " ++ encode e2
encode e = show $ toNum e

sat :: Info -> String
sat = encode . toExpr . makeGrid 

first :: Info -> String
first i = "p cnf " ++ (show $ numVar i) ++ " " ++ (show $ numClause i) ++ "\n"

numVar :: Info -> Int
numVar (Info rowSize _ colSize _) = rowSize * colSize

numClause :: Info -> Int
numClause = length . lines . sat

data Info = Info Int [[Int]] Int [[Int]]


makeGrid :: Info -> [[[Grid]]]
makeGrid (Info rowSize rows colSize cols) = condition rowSize rows ++ transposeAll (condition colSize cols)

main :: IO ()
main = do
  writeFile "question" $ concat [first info, sat info, " 0\n"]

info :: Info
info = Info 
       -- 5 [ [1,1]
       --   , [1,1]
       --   , [1,1,1]
       --   , [1,1]
       --   , [1,1]
       --   ]
       -- 5 [ [1,1]
       --   , [1,1]
       --   , [1,1,1]
       --   , [1,1]
       --   , [1,1]
       --   ]
       10 [ [4]
          , [1,1,6]
          , [1,1,4,1]
          , [1,1,4,1]
          , [1,1,4,1]
          , [1,1,3,2]
          , [1,1,4]
          , [1,1,2]
          , [1,1,2]
          , [1,1,2]
          , [1,1,2]
          ]       
       10 [ [9]
          , [0]
          , [9]
          , [0]
          , [5]
          , [7]
          , [10]
          , [5,4]
          , [2,2]
          , [5]
          ]
       -- 5 [ [5]
       --   , [0]
       --   , [5]
       --   , [2,2]
       --   , [3]         
       --   ] 
       -- 5 [ [4]
       --   , [1,3]
       --   , [3,1]
       --   , [1,3]
       --   , [4]
       --   ]



-- info2 = Info 
--         5 [ [1,1]
--           , [1,1]
--           , [1,1,1]
--           , [1,1]
--           , [1,1]
--           ]
--         5 [ [1,1]
--           , [1,1]
--           , [1,1,1]
--           , [1,1]
--           , [1,1]
--           ]






