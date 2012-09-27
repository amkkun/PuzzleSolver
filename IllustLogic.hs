{-# OPTIONS -Wall #-}

module IllustLogic where

import Formula
import Puzzle
import Lib

import Data.List

data IllustLogic = IllustLogic [[Int]] [[Int]]


-- * main
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
    
  parsePuzzle strs 
    | length col /= csize = error "IllustLogic parse error"
    | otherwise = IllustLogic row col
    where
      [rsize, csize] = map read . words . head $ strs
      (row, col) = splitAt rsize . map (map read) . map words . filter (not . null) . tail $ strs
  
  encodeType _ = Tseitin

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