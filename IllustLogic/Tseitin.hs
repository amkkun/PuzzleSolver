{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TseitinEncoding
  ( Var
  , Lit
  , Clause
  , CNF
  , Formula
  , CNFGenMonad (..)
  , emitFormula
  , CNFGen
  , runCNFGen
  , execCNFGen
  , evalCNFGen
  ) where

import Control.Monad
import Control.Monad.RWS
import Data.List

type Var = Int
data Lit = Pos Var | Neg Var deriving (Show, Eq, Ord)
type Clause = [Lit]
type CNF = [Clause]

data Formula
  = Var Var
  | And [Formula]
  | Or [Formula]
  | Not Formula
  | Imply Formula Formula
  deriving (Show, Eq, Ord)

class Monad m => CNFGenMonad m where
  newVar :: m Var
  emitClause :: Clause -> m ()

emitFormula :: CNFGenMonad m => Formula -> m ()
emitFormula x = do
  v <- enc x
  emitClause [Pos v]

enc :: CNFGenMonad m => Formula -> m Var
enc (Var v) = return v
enc (And xs) = do
  v <- newVar
  vs <- mapM enc xs
  forM vs $ \v2 -> emitClause [Neg v, Pos v2]
  emitClause (Pos v : map Neg vs)
  return v
enc (Or xs) = do
  v <- newVar
  vs <- mapM enc xs
  forM vs $ \v2 -> emitClause [Pos v, Neg v2]
  emitClause (Neg v : map Pos vs)
  return v
enc (Not x) = do
  v <- newVar
  v2 <- enc x
  emitClause [Pos v, Pos v2]
  emitClause [Neg v, Neg v2]
  return v
enc (Imply x y) = enc (Or [Not x, y])

newtype CNFGen a = CNFGen (RWS () (Endo [Clause]) Int a)
  deriving Monad

instance CNFGenMonad CNFGen where
  newVar = CNFGen $ do
    cnt <- get
    put (cnt+1)
    return cnt
  emitClause cl = CNFGen (tell (Endo (cl:)))

runCNFGen :: CNFGen x -> (x, Int, CNF)
runCNFGen (CNFGen m) =
  case runRWS m () 0 of
    (x, cnt, f) -> (x, cnt, appEndo f [])

execCNFGen :: CNFGen x -> (Int, CNF)
execCNFGen m =
  case runCNFGen m of
    (_,cnt,cnf) -> (cnt,cnf)

evalCNFGen :: CNFGen x -> (x, CNF)
evalCNFGen m = 
  case runCNFGen m of
    (x,_,cnf) -> (x,cnf)

test :: (Int, CNF)
test = execCNFGen $ do
  x <- newVar
  y <- newVar
  z <- newVar
  emitFormula $ constNoUE info -- And [Or [Var x, Var y] , Var z]
{-
=>
(5
, [ [Pos 4,Neg 0]
  , [Pos 4,Neg 1]
  , [Neg 4,Pos 0,Pos 1]
  , [Neg 3,Pos 4]
  , [Neg 3,Pos 2]
  , [Pos 3,Neg 4,Neg 2]
  , [Pos 3]
  ]
)
-}


-- illustlogic

data Cell = Cell Int Bool deriving (Show, Eq)

distribute :: Int -> Int -> [[Int]] -- n wo k ko no kumi ni wakeru. 
distribute n size 
  | size <= 0 = []
  | size == 1 = [[n]]
  | otherwise = [m : d | m <- [0..n], d <- distribute (n - m) (size - 1)]

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


constNoUE :: Info -> Formula
constNoUE (Info rows cols) = 
  And $ row rowSize rows ++ col colSize cols
  where
    rowSize = length cols
    colSize = length rows 
 
row :: Int -> [[Int]] -> [Formula]
row size conds = zipWith one 
                 (takeRepeat size [1..(size * length conds)]) 
                 (map (constraint size) conds)
col :: Int -> [[Int]] -> [Formula]
col size conds = zipWith one 
                 (transpose $ takeRepeat size [1..(size * length conds)]) 
                 (map (constraint size) conds)
  

-- one [1,2,3] [[True,False,False],[False,True,True]] == Or (And (Var ) (Or (Var) (Const False))  
one :: [Int] -> [[Bool]] -> Formula
one ns = Or . map three . map (two' ns)

two' :: [Int] -> [Bool] -> [Cell]
two' ns bs = zipWith Cell ns bs
 
three :: [Cell] -> Formula
three = And . map (\(Cell n bool) -> if bool then Var n else Not (Var n)) 



data Info = Info [[Int]] [[Int]]
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
