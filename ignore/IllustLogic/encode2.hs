{-# OPTIONS -Wall #-}

module Main where

import Formula
import Data.List 

data Cell = Cell Int Bool deriving (Show, Eq)

-- (n - sum l - (length l - 1)) 個のものを (length l + 1) つの組にわける



-- sum (distribute n k) == n
-- distribute 2 3 = [[2,0,0],[1,1,0],[1,0,1],[0,2,0],[0,1,1],[0,0,2]]
distribute :: Int -> Int -> [[Int]] -- n wo k ko no kumi ni wakeru. 
distribute n size 
  | size <= 0 = []
  | size == 1 = [[n]]
  | otherwise = [m : d | m <- [0..n], d <- distribute (n - m) (size - 1)]

-- length l + 1 == length d
    
    

-- line :: Int -> [[Bool]] -> [[Cell]] -- line number -> cellss -> gridss
-- line n = map (\c -> [Grid n i (c !! pred i) | i <- [1..length c]])

-- grid :: [[[Bool]]] -> [[[Cell]]]
-- grid c = [line i (c !! pred i) | i <- [1..length c]]

-- condition :: Int -> [[Int]] -> [[[Cell]]]
-- condition size = grid . map (constraint size) 

-- transpose :: Cell -> Cell
-- transpose (Grid r c cell) = Grid c r cell

-- transposeAll :: [[[Grid]]] -> [[[Grid]]]
-- transposeAll = map (map (map Main.transpose))

-- toExpr :: [[[Grid]]] -> Expr Grid
-- toExpr = allAnd . map toCnf . map allOr . map (map allAnd) . map (map (map expr))
--   where
--     expr g = Var g
    
--

constraint :: Int -> [Int] -> [[Bool]]
constraint size fs
  | b < 0 = error ("constraint: " ++ show fs)
  | otherwise = distribute b (length fs + 1) >>= 
                return . bools fs' . map (`replicate` False)    
  where
    b = size - sum fs - (length fs - 1) -- 列の長さ - 連続して塗りつぶす数の合計 - 間の空白の数 = 残りの空白の数
    fs' = map (`replicate` True) fs

-- 交互に取る
mutually :: [a] -> [a] -> [a]
mutually [] _ = []
mutually (x:_) [] = [x]
mutually (x:xs) (y:ys) = x : y : mutually xs ys

-- n個とってきてconcat、n個とってきてconcat…
takeConcat :: Int -> [[a]] -> [[a]]
takeConcat _ [] = []
takeConcat n xss = concat (take n xss) : takeConcat n (drop n xss)

bools :: [[Bool]] -> [[Bool]] -> [Bool]
bools ts = concat . intersperse [False] . takeConcat 2 . mutually ts


constNoUE :: Info -> Formula Cell
constNoUE (Info rows cols) = 
  allAnd $ map cnf (row rowSize rows) ++ map cnf (col colSize cols)
  where
    rowSize = length cols
    colSize = length rows 
 
row :: Int -> [[Int]] -> [Formula Cell]
row size conds = zipWith one 
                 (takeAll size [1..(size * length conds)]) 
                 (map (constraint size) conds)
col :: Int -> [[Int]] -> [Formula Cell]
col size conds = zipWith one 
                 (transpose $ takeAll size [1..(size * length conds)]) 
                 (map (constraint size) conds)
  
-- transpose :: [[a]] -> [[a]]
-- transpose [] = []
-- transpose [xs] = [[x] | x <- xs]
-- transpose (xs:xss) = zipWith (:) xs (transpose xss)


-- one [1,2,3] [[True,False,False],[False,True,True]] == Or (And (Var ) (Or (Var) (Const False))  
one :: [Int] -> [[Bool]] -> Formula Cell
one ns = allOr . map three . map (two' ns)
  
-- two 3 [True, False, True] == [Cell 7 True, Cell 8 False, Cell 9 True] 
two :: Int -> [Bool] -> [Cell]
two n bs = zipWith Cell [m..] bs
  where
    m = (n - 1) * length bs + 1

two' :: [Int] -> [Bool] -> [Cell]
two' ns bs = zipWith Cell ns bs
 
three :: [Cell] -> Formula Cell
three = allAnd . map Var 

zipFunc :: [a -> b] -> [a] -> [b]
zipFunc [] _ = []
zipFunc _ [] = []
zipFunc (f:fs) (x:xs) = f x : zipFunc fs xs

takeAll :: Int -> [a] -> [[a]]
takeAll _ [] = []
takeAll n xs = take n xs : takeAll n (drop n xs)
-- 
--

--        1 
--       21113
--   2 1 **..*
-- 1 1 1 *.*.*
--   1 2 .*.**

rsize :: Info -> Int
rsize (Info rows _) = length rows

csize :: Info -> Int 
csize (Info _ cols) = length cols

toNum :: Formula Cell -> Int -- var -> num
toNum (Var (Cell n bool)) = if bool then n else negate n
toNum (Not v) = negate $ toNum v
toNum _ = error "toNum: It is NOT variable."


encode :: Formula Cell -> [[Int]] -- CNF 
encode (And p q) = encode p ++ encode q
encode (Or p q) = [p' ++ q' | p' <- encode p, q' <- encode q]
encode (Const _) = []
encode v = [[toNum v]]



showCnf :: [[Int]] -> String
showCnf = unlines . map (concat . intersperse " " . map show . (++ [0]))

sat :: Info -> String
sat = showCnf . encode . constNoUE

first :: Info -> String
first i = "p cnf " ++ (show $ numVar i) ++ " " ++ (show $ numClause i) ++ "\n"

numVar :: Info -> Int
numVar (Info rows cols) = length rows * length cols

numClause :: Info -> Int
numClause = length . lines . sat

data Info = Info [[Int]] [[Int]]


-- makeGrid :: Info -> [[[Grid]]]
-- makeGrid (Info rowSize rows colSize cols) = condition rowSize rows ++ transposeAll (condition colSize cols)

main :: IO ()
main = do
  writeFile "question" $ concat [first info, sat info, " 0\n"]

info :: Info
info = Info 
       [ [1,1]
       , [1,1]
       , [2]
       , [1]
       ]
       [ [2]
       , [1]
       , [1,1]
       , [0]
       ]
       -- [ [4]
       -- , [1,1,6]
       -- , [1,1,4,1]
       -- , [1,1,4,1]
       -- , [1,1,4,1]
       -- , [1,1,3,2]
       -- , [1,1,4]
       -- , [1,1,2]
       -- , [1,1,2]
       -- , [1,1,2]
       -- , [1,1,2]
       -- ]       
       -- [ [9]
       -- , [0]
       -- , [9]
       -- , [0]
       -- , [5]
       -- , [7]
       -- , [10]
       -- , [5,4]
       -- , [2,2]
       -- , [5]
       -- ]
       
