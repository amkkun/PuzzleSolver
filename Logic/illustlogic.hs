{-# OPTIONS -Wall #-}

import Logic
import Data.List
import Control.Applicative ((<$>))

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

info2 :: Info
info2 = Info
        [ [1,1]
        , [1,1]
        , [2,3]
        , [4,5]
        , [10]
        , [8]
        , [0]
        , [0]
        , [0]
        , [0]
        , [1,1]
        , [10]
        , [8]
        , [4]
        , [2]
        ]
        [ [2,2]
        , [4,2]
        , [5,2]
        , [1,3,3]
        , [2,4]
        , [3,4]
        , [4,3]
        , [6,2]
        , [4,2]
        , [2,2]
        ]


small :: Info
small = Info
        [ [1]
        , [2]
        , [3]
        ]
        [ [1]
        , [3]
        , [2]
        ]
        
small2 :: Info
small2 = Info
         [ [1,2]
         ]
         [ [1]
         , [0]
         , [1]
         , [1]
         -- , [0]
         -- , [1]
         -- , [1]
         -- , [1]
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
divide :: Int -> [a] -> [[a]]
divide _ [] = []
divide n xs = let (left, right) = splitAt n xs in left : divide n right


-- # main
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
  | otherwise = allOr [makeFormula vs ns bs' | bs <- distribute rest blankLen
                                            , let bs' = betweenPlus bs] 
  where
    len = length vs 
    rest = len - sum ns - (length ns - 1)
    blankLen = if ns == [0] then 1 else length ns + 1
      
-- makeFormula [1,2,3,4] [1,2] [0,1,0] == And (Var 1) (And (Not (Var 2)) (And (Var 3) (Var 4)))
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
    
betweenPlus :: [Int] -> [Int]
betweenPlus xs
  | null xs || length xs == 1 = xs
  | otherwise = head xs : map succ (init $ tail xs) ++ [last xs]
      

varMatrix :: Info -> Matrix VarNum
varMatrix (Info xss yss) = divide (length yss) [1..(length xss * length yss)]

-- # IO
useTseitin :: Info -> IO ()
useTseitin = writeFile "qT.dimacs" . dimacsTseitin . constraint 

useNormal :: Info -> IO ()
useNormal = writeFile "qN.dimacs" . dimacsNormal . constraint 

decode :: FilePath -> Info -> IO ()
decode filepath (Info rs cs) = do
  ans <- lines <$> readFile filepath
  if head ans == "SAT"
    then display . ready (length cs) . take (length rs * length cs) . map read . words $ ans !! 1
    else putStrLn "It is not satisfiable!"
  where       
    ready size = divide size . map (\n -> if n > 0 then True else False)
    display = mapM_ (putStrLn . concat . map (\b -> if b then "#" else " ")) 
  