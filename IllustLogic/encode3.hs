{-# OPTIONS -Wall #-}

module Main where

import DefaultPropLogic
import PropLogicCore
import PropLogicTest
import Data.List 

import Debug.Trace

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
--

constraint :: Int -> [Int] -> [[Bool]]
constraint size fs
  | fs == [] = error "constraint: empty list. possible fix -> [0]"
  | b < 0 = error ("constraint: " ++ show size ++ " < " ++ show fs)
  | otherwise = do
      fs' <- distribute b len
      let fs'' = map (`replicate` False) fs'
      return $ bools fs'' ts
  where
    b = size - sum fs - (length fs - 1) -- 列の長さ - 連続して塗りつぶす数の合計 - 間の空白の数 = 残りの空白の数
    len = if fs == [0] then 1 else length fs + 1
    ts = map (`replicate` True) fs

-- 交互に取る
mutually :: [a] -> [a] -> [a]
mutually [] _ = []
mutually (x:_) [] = [x]
mutually (x:xs) (y:ys) = x : y : mutually xs ys

-- n個とってきてconcat、n個とってきてconcat…
takeConcat :: Int -> [[a]] -> [[a]]
takeConcat n = map concat . takeRepeat n -- _ [] = []
-- takeConcat n xss = concat (take n xss) : takeConcat n (drop n xss)

takeRepeat :: Int -> [a] -> [[a]]
takeRepeat _ [] = []
takeRepeat n xs = take n xs : takeRepeat n (drop n xs)

-- [[True],[True,True]] [[False],[],[False]] == 
--   [False,True,False,True,True,False]
bools :: [[Bool]] -> [[Bool]] -> [Bool]
bools fs = concat . mutually fs . takeConcat 2 . intersperse [False]


constNoUE :: Info -> PCNF Int
constNoUE (Info rows cols) = 
  primeCNF $ CJ $ row rowSize rows ++ col colSize cols
  where
    rowSize = length cols
    colSize = length rows 
 
row :: Int -> [[Int]] -> [PropForm Int]
row size conds = zipWith one 
                 (takeRepeat size [1..(size * length conds)]) 
                 (map (constraint size) conds)
col :: Int -> [[Int]] -> [PropForm Int]
col size conds = zipWith one 
                 (transpose $ takeRepeat size [1..(size * length conds)]) 
                 (map (constraint size) conds)
  

-- one [1,2,3] [[True,False,False],[False,True,True]] == Or (And (Var ) (Or (Var) (Const False))  
one :: [Int] -> [[Bool]] -> PropForm Int
one ns = DJ . map three . map (two' ns)
  
-- two 3 [True, False, True] == [Cell 7 True, Cell 8 False, Cell 9 True] 
-- two :: Int -> [Bool] -> [Cell]
-- two n bs = zipWith Cell [m..] bs
--   where
--     m = (n - 1) * length bs + 1

two' :: [Int] -> [Bool] -> [Cell]
two' ns bs = zipWith Cell ns bs
 
three :: [Cell] -> PropForm Int
three = CJ . map (\(Cell n bool) -> if bool then A n else N (A n)) 

zipFunc :: [a -> b] -> [a] -> [b]
zipFunc [] _ = []
zipFunc _ [] = []
zipFunc (f:fs) (x:xs) = f x : zipFunc fs xs

-- 
--


-- rsize :: Info -> Int
-- rsize (Info rows _) = length rows

-- csize :: Info -> Int 
-- csize (Info _ cols) = length cols

toNum :: PCNF Int -> Int -- var -> num
toNum (A n) = n
toNum (N v) = negate $ toNum v
toNum _ = error "toNum: It is NOT variable."

oror :: PCNF Int -> [Int]
oror (DJ p) = map toNum p
oror T = []
oror F = [] 
oror v = [toNum v]

encode :: PCNF Int -> [[Int]] -- CNF 
encode (CJ p) = map oror p
encode (DJ p) = [oror (DJ p)] -- concat [encode p' | p' <- p]
encode T = []
encode F = []
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
       -- [ [1,1]
       -- , [1,1]
       -- , [2]
       -- , [1]
       -- ]
       -- [ [2]
       -- , [1]
       -- , [1,1]
       -- , [0]
       -- ]
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
       [ [1,1,1,1,1,1] 
       ]  
       [ [0]
       , [0]
       , [0]
       , [0]
       , [0]
       , [0]
       , [0]
       , [0]
       , [0]
       , [0]
       , [0]
       , [0]
       , [0]
       , [0]
       , [0]
       , [0]
       ]
