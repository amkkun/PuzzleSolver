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
import qualified Data.Map as Map
import Control.Applicative
import Control.Monad

type RowId = Int
type ColId = Int
-- type BoxId = Int
type Pos = (RowId, ColId)
type Number = Int
type Board = Map.Map Pos Number
type SubBoard = Map.Map Pos Number -- row, col, box

emptyNum :: Number
emptyNum = 0

boxSize :: Int
boxSize = 3
boardSize :: Int
boardSize = boxSize ^ (2 :: Int) 

-- 数独を最後まで解く。考えられるすべての解をだす。
solve :: Board -> [Board]
solve board 
  | finish board = return board
  | otherwise = update board >>= solve

-- 終了(全部埋まっているかどうか)かどうか判断
finish :: Board -> Bool
finish = Map.null . Map.filter (== emptyNum)

-- 数独を一段階解く。
-- まだ完成ではないという前提(0がある)
-- ([数字] >>= 空いているマスに、制約条件を満たす数字を入れる)
update :: Board -> [Board]
update board = do
    num <- [1..boardSize]
    let new = Map.adjust (const num) emptyPos board 
    if constraint new emptyPos
        then return new
        else []
  where
    emptyPos = head . Map.keys $ Map.filter (== emptyNum) board -- このheadのところを変えると高速化できそう    
        
-- 盤は制約条件を満たすか？(あるマスに注目して)
constraint :: Board -> Pos -> Bool 
constraint board pos = all check subBoards
  where 
    subBoards = map (getSubBoard board pos) [getRowId, getColId, getBoxId]

-- 空のもの以外で重複しているものがないかどうかチェック
check :: SubBoard -> Bool
check board = nub foo == foo
  where 
    foo = Map.elems $ Map.filter (/= emptyNum) board 

-- 行、列、箱を得る
getSubBoard :: Board -> Pos -> (Pos -> Int) -> SubBoard
getSubBoard board pos getId = 
  Map.filterWithKey (\p _ -> getId p == getId pos) board

-- その位置の行番号、列番号、箱番号
getRowId, getColId, getBoxId :: Pos -> Int
getRowId (r, _) = r 
getColId (_, c) = c
getBoxId (r, c) = r' * boxSize + c'
  where
    r' = (r - 1) `div` boxSize   -- 0 1 2
    c' = (c - 1) `div` boxSize + 1 -- 1 2 3

 
toBoard :: [[Number]] -> Board 
toBoard nums = toBoard' (1, 1)
  where
    toBoard' (r, c) 
      | r == boardSize && c > boardSize = Map.empty
      | c > boardSize = toBoard' (r + 1, 1)
      | otherwise = Map.singleton (r, c) (nums !! (r - 1) !! (c - 1)) `Map.union` toBoard' (r, c + 1)

toList :: Board -> [[Number]]
toList board = toList' 1
  where
    toList' n
      | n > boardSize = []
      | otherwise = Map.elems (Map.filterWithKey (\(r, _) _ -> r == n) board) : toList' (n + 1)

main :: IO ()
main = do
  nums <- forM [1..boardSize] (\_ -> map read . words <$> getLine)
  let boards = map toList $ solve $ toBoard nums
  mapM_ (mapM_ (putStrLn . intersperse ' ' . concat . map show)) boards 
  

test :: Board
test = toBoard testNums
  where
    testNums = [ [0, 0, 1, 2]
               , [1, 2, 0, 0]
               , [2, 1, 0, 0]
               , [0, 0, 2, 1]
               ]
