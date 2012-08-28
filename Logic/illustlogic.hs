{-# OPTIONS -Wall #-}

import Logic
import Data.List

data Cell = Cell Int Bool
data Info = Info [[Int]] [[Int]]
type Matrix a = [[a]]

info :: Info
info = Info
       [ [0]
       , [1,2]
       , [2,1]
       , [1,2]
       , [3,1,3]
       , [8,1]
       , [10,1]
       , [12]
       , [10,1]
       , [12]
       , [12]  
       , [12]
       , [10]
       , [3,3]
       , [0]
       ] 
       [ [0]
       , [0]
       , [7]
       , [9]
       , [10]
       , [10]
       , [9]
       , [12]
       , [1,8]
       , [1,1,9]
       , [1,2,8]
       , [1,1,8]
       , [1,1,4]
       , [7] 
       , [0]
       ]

-- distribute 2 3 == [[2,0,0],[1,1,0],[1,0,1],[0,2,0],[0,1,1],[0,0,2]]
distribute :: Int -> Int -> [[Int]]
distribute n size 
  | size <= 0 = []
  | size == 1 = [[n]]
  | otherwise = [m : d | m <- [0..n], d <- distribute (n - m) (size - 1)]

-- mutually [1,2,3,4] [7,8,9] == [1,7,2,8,3,9,4]
mutually :: [a] -> [a] -> [a]
mutually [] _ = []
mutually (x:_) [] = [x]
mutually (x:xs) (y:ys) = x : y : mutually xs ys

-- divide 3 [1..10] == [[1,2,3],[4,5,6],[7,8,9],[10]]
-- divide :: Int -> [a] -> [[a]]
-- divide _ [] = []
-- divide n xs = let (left, right) = splitAt n xs in left : divide n right



constraint :: Info -> Formula
constraint info@(Info xss yss) = 
  And 
  (allAnd [lineConstraint vs xs | (vs, xs) <- zip vss xss])  
  (allAnd [lineConstraint vs ys | (vs, ys) <- zip (transpose vss) yss])
  where
    vss = varMatrix info
    
-- 
lineConstraint :: [VarNum] -> [Int] -> Formula
lineConstraint vs ns
  | ns == [] = error "constraint: empty list. possible fix -> [0]"
  | rest < 0 = error ("constraint: " ++ show len ++ " < " ++ show ns)
  | otherwise = allOr [makeFormula vs ns bs | bs <- distribute rest blankLen] 
  where
    len = length vs 
    rest = len - sum ns - (length ns - 1)
    blankLen = if ns == [0] then 1 else length ns + 1
      
-- foo [1,2,3,4] [1,1,2] == And (Not (Var 1)) (And (Var 2) (And (Not (Var 3)) (And (Not (Var 4)) (Const True))))
makeFormula :: [VarNum] -> [Int] -> [Int] -> Formula
makeFormula vs ns bs = makeFormula' vs (mutually bs ns) False
  where
    makeFormula' _ [] _ = Const True
    makeFormula' vs (c:cs) bool
      | bool = And (allAnd vals) (makeFormula' right cs False)
      | otherwise = And (allAnd $ map Not vals) (makeFormula' right cs True)
      where
        (left, right) = splitAt c vs
        vals = map Var left
        
varMatrix :: Info -> Matrix VarNum
varMatrix (Info xss yss) = divide (length yss) [1..(length xss * length yss)]

