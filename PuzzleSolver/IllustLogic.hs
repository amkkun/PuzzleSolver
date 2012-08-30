{-# OPTIONS -Wall #-}

module IllustLogic where

import Formula
import Puzzle
import Lib

import Data.List

data IllustLogic = IllustLogic [[Int]] [[Int]]

info :: IllustLogic
info = IllustLogic
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

midium :: IllustLogic
midium = IllustLogic
         [ [3,3,7,3,4,2]
         , [3,2,6,2,4,2]
         , [5,1,5,2,3]
         , [4,3,1,3]
         , [3,1,5]
         
         , [3,3]
         , [1,2,1,1,2]
         , [3,2,3,3,3]
         , [5,2,2,3,1]
         , [10,3,8,2]
           
         , [1,5,2,2,4,3]
         , [2,2,2,1,1,2,2,4]
         , [1,1,1,2,2,2,2,2,4]
         , [1,2,2,2,2,4,2]
         , [3,5,1,5,1,1,1]
           
         , [2,1,2,1,3]
         , [2,1,2,1,2,3]
         , [1,2,1,2,2,3]
         , [2,1,2,2,1,3,1]
         , [3,2,2,3,5]
           
         , [3,4,3,6]
         , [4,1,5,6,5]
         , [3,3,12,1,4,1]
         , [5,4,8,3,5]
         , [3,1,2,2,1,1,4,5]
           
         , [2,2,2,9,1,3]
         , [3,1,2,3,5]
         , [3,2,12,1,5]
         , [1,2,2,10,2,3,2]
         , [4,2,1,5,1]
         ]
         [ [3,3,1,2,6,2,3]
         , [6,3,1,1,1,10,1]
         , [16,11]
         , [5,3,5,3,2]
         , [4,5,4,3,1]
         , [2,2,5,4,3]
         , [1,2,2,1,3,2]
         , [1,2,1,6,2]
         , [1,2,1,1,1,3,1]
         , [1,1,1,4,3]
         , [3,1,2,2,2,2,2]
         , [2,1,4,3,1,2]
         , [3,2,1,4,4,2]
         , [4,2,2,2,2,1,2]
         , [5,1,2,1,2]
         , [4,2,2,2,4,2]
         , [1,2,1,4,2,1,2]
         , [2,1,4,3,1,2]
         , [2,1,2,2,5,2]
         , [1,1,1,2,1,2]
         , [1,2,1,5,1,2]
         , [1,2,1,3,6]
         , [2,2,2,2,5]
         , [5,7,3,1]
         , [3,10,2,3,2]
         , [2,4,3,4,12]
         , [4,2,13]
         , [5,18,1]
         , [3,5,3,3,2,3]
         , [5,3,3,3,4]
         ]

mid :: IllustLogic
mid = IllustLogic
      [ [1,1]
      , [1]
      , [10]
      , [6,6]
      , [3,4]
      , [3,4]
      , [4,2,1]
      , [1,2,3,2,1]
      , [2,1,1,2]
      , [2,1,1,1,1,2]
      , [1,1,1,1]
      , [1,1,7,1,1]
      , [1,1,1,1,1,1]
      , [2,3,2,2]
      , [2,3]
      , [2,2]
      , [7,5,6]
      , [7,5,6]
      , [6,3,5]
      , [5,1,1,1,1,1,4]
      ]
      [ [3,4,4]
      , [3,2,1,4]
      , [6,4,4]
      , [1,1,6]
      , [1,2,5]
      , [2,2,3,3]
      , [1,1,2,2,1]
      , [1,1,1,1,1]
      , [1,1,1,1,1,2,1]
      , [2,1,1,1,1,3]
      , [1,1,1,2,4]
      , [1,3,3]
      , [1,2,2,1]
      , [1,2]
      , [2,3,3,1]
      , [1,6]
      , [6,1,4]
      , [3,2,2,4]
      , [3,5,4]
      , [3,4]
      ]
info2 :: IllustLogic
info2 = IllustLogic
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


small :: IllustLogic
small = IllustLogic
        [ [1]
        , [2]
        , [3]
        ]
        [ [1]
        , [3]
        , [2]
        ]
        
small2 :: IllustLogic
small2 = IllustLogic
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



-- # main
instance Puzzle IllustLogic where
  maxVariable (IllustLogic xss yss) = length xss * length yss 
  
  varMatrix (IllustLogic xss yss) = 
    [divide (length yss) [1..(length xss * length yss)]]
  
  constraint illust@(IllustLogic xss yss) = 
    And 
    (allAnd [lineConstraint vs xs | (vs, xs) <- zip vss xss])  
    (allAnd [lineConstraint vs ys | (vs, ys) <- zip (transpose vss) yss])
    where
      vss = head $ varMatrix illust
  
  showPuzzle (IllustLogic _ yss) = 
    mapM_ (putStrLn . concat . map (\b -> if b then "#" else " ")) .
    divide (length yss) . 
    map (\n -> if n > 0 then True else False)

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
makeFormula vars ns bs = makeFormula' vars (mutually bs ns) False
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
      



-- # IO
-- useTseitin :: IllustLogic -> IO ()
-- useTseitin = C.writeFile "qT.dimacs" . dimacsTseitin . constraint 

-- useNormal :: IllustLogic -> IO ()
-- useNormal = C.writeFile "qN.dimacs" . dimacsNormal . constraint 

-- decode :: FilePath -> IllustLogic -> IO ()
-- decode filepath (IllustLogic rs cs) = do
--   ans <- lines <$> readFile filepath
--   if head ans == "SAT"
--     then display . ready (length cs) . take (length rs * length cs) . map read . words $ ans !! 1
--     else putStrLn "It is not satisfiable!"
--   where       
--     ready size = divide size . map (\n -> if n > 0 then True else False)
--     display = mapM_ (putStrLn . concat . map (\b -> if b then "#" else " ")) 

-- -- encode :: FilePath -> IO ()
-- -- encode filepath = do
-- --   question <- lines <$> readFile filepath
-- --   (rsize, 

-- main :: IO ()
-- main = useTseitin mid