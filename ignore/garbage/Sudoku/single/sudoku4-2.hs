{-# OPTIONS -Wall #-}

-- リストモナドのみで簡潔なバックトラックを記述できる
-- どれから行くか。いれられるものがすくないものから
-- 枝刈り

-- 制約条件は？
-- 行・列・箱で数字が被らない
-- 行・列・箱で1からマスの数まで入る
-- マスには必ずひとつ数字が入る
-- Minimal Complete？

module Main where

import Data.List
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Traversable as T
import Control.Applicative
import Control.Monad


emptyNum :: Int
emptyNum = 0

boxSize :: Int
boxSize = 3
boardSize :: Int
boardSize = boxSize ^ (2 :: Int) 


data Sub = Row | Col | Box deriving (Eq, Show)
data SubID = SubID { getSub :: Sub  
                   , getID :: ID 
                   } deriving (Eq, Show)

type ID = Int
type Pos = (ID, ID) -- (Row, Col)

type ABoard a = Map.Map Pos a -- Abstract Board
type Board = ABoard Int -- data Board = Board Size (Map.Map Pos Int)
type PBoard = ABoard [Int] -- Possible





getSubBoard :: ABoard a -> SubID -> ABoard a
getSubBoard b s = Map.filterWithKey (\p _ -> toSubID p (getSub s) == s) b

toSubID :: Pos -> Sub -> SubID
toSubID (r, _) Row = SubID Row r
toSubID (_, c) Col = SubID Col c
toSubID (r, c) Box = SubID Box $ r' * boxSize + c'
  where
    r' = (r - 1) `div` boxSize   -- 0 1 2
    c' = (c - 1) `div` boxSize + 1 -- 1 2 3

-- 入る可能性のある数のリスト
-- すでに数字が入っていたり入る可能性のあるものがなかったら空リスト
possibleNums :: Board -> Pos -> [Int]
possibleNums b p
  | decided /= 0 = return decided
  | otherwise = filter (`notElem` decidedNums) [1..boardSize]
  where
    decided = fromJust $ Map.lookup p b -- pに変な数字は来ないと仮定
    decidedNums = filter (/= emptyNum) . Map.elems $ getRCB b p

-- その位置を除いた、行・列・箱を得る
getRCB :: ABoard a -> Pos -> ABoard a
getRCB b p = Map.delete p . Map.unions $ 
               getSubBoard b <$> toSubID p <$> [Row, Col, Box] 


-- 数独を最後まで解く。考えられるすべての解をだす。
solve :: Board -> [Board]
solve b
  | isFailed b = []
  | isCompleted b = return b
  | otherwise = update b >>= solve . prune
   
-- 終了(全部埋まっているかどうか)かどうか判断
isCompleted :: Board -> Bool
isCompleted = Map.null . Map.filter (== emptyNum)

isFailed :: Board -> Bool
isFailed b = 

-- 数独を一段階解く。
-- まだ完成ではないという前提(0がある)
-- ([条件を満たす数字] >>= 空いているマスに入れる)
update :: Board -> [Board]
update b = possibleNums b emptyPos >>= return . renew b emptyPos 
  where
    emptyPos = head . Map.keys $ Map.filter (== emptyNum) b -- ここを変えると高速化できそう    

renew :: Board -> Pos -> Int -> Board
renew b p n = Map.adjust (const n) p b
    
--

decide :: PBoard -> Pos -> PBoard
decide b p = if null bools then b else Map.adjust (const bools) p b
  where
    rcb = Map.elems $ getRCB b p
    bools = do
      n <- [1..boardSize]
      let bool = and $ elem n <$> rcb
      if bool 
        then [n]
        else []
    
decideAll :: PBoard -> PBoard
decideAll b = foldl' decide b (Map.keys b) 

moreDecide :: PBoard -> PBoard
moreDecide b
  | b == b' = b
  | otherwise = moreDecide b'
  where
    b' = decideAll b

back :: PBoard -> Board    
back b = Map.map foo $ b
  where
    foo ns = if length ns == 1 then head ns else emptyNum

possibleMap :: Board -> PBoard
possibleMap b = Map.mapWithKey (\p _ -> possibleNums b p) b    


one :: Board -> Board
one = back . possibleMap  


update' :: Board -> [Board]
update' b = T.sequence pMap 
  where
    pMap = possibleMap b


prune :: Board -> Board
prune b 
  | b == b' = b
  | otherwise = prune b'
  where
    b' = one b

-- 
toBoard :: [[Int]] -> Board 
toBoard nums = toBoard' (1, 1)
  where
    toBoard' (r, c) 
      | r == boardSize && c > boardSize = Map.empty
      | c > boardSize = toBoard' (r + 1, 1)
      | otherwise = Map.singleton (r, c) (nums !! (r - 1) !! (c - 1)) `Map.union` toBoard' (r, c + 1)

--
toList :: Board -> [[Int]]
toList board = toList' 1
  where
    toList' n
      | n > boardSize = []
      | otherwise = Map.elems (Map.filterWithKey (\(r, _) _ -> r == n) board) : toList' (n + 1)


showBoard :: Board -> IO ()
showBoard b = (putStrLn . intersperse ' ' . concat . map show) `mapM_` toList b 

solveS :: Board -> IO ()
solveS b = mapM_ showBoard $ solve b

main :: IO ()
main = do
  nums <- forM [1..boardSize] (\_ -> map read . words <$> getLine)
  let boards = solve $ toBoard nums
  mapM_ showBoard boards 
  

test :: Board
test = toBoard testNums
  where
    testNums = [ [0, 0, 1, 2]
               , [1, 2, 0, 0]
               , [2, 1, 0, 0]
               , [0, 0, 2, 1]
               ]
test2 :: Board
test2 = toBoard testNums
  where
    testNums = [ [2, 5, 0, 0, 3, 0, 0, 4, 6]
               , [0, 0, 9, 0, 2, 0, 0, 0, 8]
               , [4, 3, 0, 7, 6, 1, 0, 0, 9]
               , [0, 0, 0, 6, 0, 0, 0, 0, 0]
               , [1, 0, 0, 9, 8, 4, 0, 0, 5]
               , [0, 0, 0, 0, 0, 2, 0, 0, 0]
               , [3, 0, 0, 1, 4, 8, 0, 7, 2]
               , [8, 0, 0, 0, 7, 0, 9, 0, 0]
               , [7, 4, 0, 0, 9, 0, 0, 5, 3]
               ]

test3 :: Board
test3 = toBoard testNums
  where
    testNums = [ [8, 0, 0, 0, 0, 0, 0, 0, 0]
               , [0, 0, 3, 6, 0, 0, 0, 0, 0]
               , [0, 7, 0, 0, 9, 0, 2, 0, 0]
               , [0, 5, 0, 0, 0, 7, 0, 0, 0]
               , [0, 0, 0, 0, 4, 5, 7, 0, 0]
               , [0, 0, 0, 1, 0, 0, 0, 3, 0]
               , [0, 0, 1, 0, 0, 0, 0, 6, 8]
               , [0, 0, 8, 5, 0, 0, 0, 1, 0]
               , [0, 9, 0, 0, 0, 0, 4, 0, 0]
               ]
