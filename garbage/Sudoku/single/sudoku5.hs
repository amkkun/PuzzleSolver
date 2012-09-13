{-# OPTIONS -Wall #-}

-- リストモナドのみで簡潔なバックトラックを記述できる
-- どれから行くか。いれられるものがすくないものから
-- 枝刈り

-- 制約条件は？
-- 行・列・箱で数字が被らない
-- 行・列・箱で1からマスの数まで入る
-- マスには必ずひとつ数字が入る
-- Minimal Complete？

-- 箱のなかで、ある行または列しかない→その行・列はその数字を消せる

module Main where

import Data.List
import qualified Data.Map as Map
import Control.Applicative
import Control.Monad
import Data.Maybe

type RowId = Int
type ColId = Int
-- type BoxId = Int
type Pos = (RowId, ColId)
type Number = Int
type Board = Map.Map Pos [Number]
type SubBoard = Map.Map Pos [Number] -- row, col, box

-- emptyNum :: Number
-- emptyNum = 0

boxSize :: Int
boxSize = 3
boardSize :: Int
boardSize = boxSize ^ (2 :: Int) 

-- -- 数独を最後まで解く。考えられるすべての解をだす。
-- solve :: Board -> [Board]
-- solve board 
--   | finish board = return board
--   | otherwise = update board >>= solve

-- -- 終了(全部埋まっているかどうか)かどうか判断
-- finish :: Board -> Bool
-- finish = Map.null . Map.filter (\ns -> length ns <= 1) -- length ns == 1?

-- -- 数独を一段階解く。
-- -- まだ完成ではないという前提(0がある)
-- -- ([数字] >>= 空いているマスに、制約条件を満たす数字を入れる)
-- update :: Board -> [Board]
-- update board = do
--   num <- minLenNums
--   let new = reload (Map.adjust (const [num]) emptyPos board) emptyPos  
--   return new -- minLenNumsが空だった場合は空に
--   where
--     minLenNums = minimumBy (\a b -> compare (length a) (length b)) $ Map.elems board
--     emptyPos = head . Map.keys $ Map.filter (== minLenNums) board -- 考えられる数字の数が少ないものから

-- -- 行・列・箱でその数字をなくす。渡される位置は決定している前提
-- reload :: Board -> Pos -> Board
-- reload board pos = board -- Map.adjust f 
--   where 
--     num = head $ Map.findWithDefault [0] pos board
--     f xs = delete num xs 
--     -- subset = 
    
-- -- 盤は制約条件を満たすか？(あるマスに注目して)
-- constraint :: Board -> Pos -> Bool 
-- constraint board pos = all check subBoards
--   where 
--     subBoards = map (getSubBoard board pos) [getRowId, getColId, getBoxId]

-- -- 空のもの以外で重複しているものがないかどうかチェック
-- check :: SubBoard -> Bool
-- check board = nub foo == foo
--   where 
--     foo = Map.elems $ Map.filter (/= []) board 

-- -- 行、列、箱を得る
-- getSubBoard :: Board -> Pos -> (Pos -> Int) -> SubBoard
-- getSubBoard board pos getId = 
--   Map.filterWithKey (\p _ -> getId p == getId pos) board

-- -- その位置の行番号、列番号、箱番号
-- getRowId, getColId, getBoxId :: Pos -> Int
-- getRowId (r, _) = r 
-- getColId (_, c) = c
-- getBoxId (r, c) = r' * boxSize + c'
--   where
--     r' = (r - 1) `div` boxSize   -- 0 1 2
--     c' = (c - 1) `div` boxSize + 1 -- 1 2 3

 
-- toBoard :: [[Number]] -> Board 
-- toBoard nums = toBoard' (1, 1)
--   where
--     toBoard' (r, c) 
--       | r == boardSize && c > boardSize = Map.empty
--       | c > boardSize = toBoard' (r + 1, 1)
--       | otherwise = Map.singleton (r, c) (nums !! (r - 1) !! (c - 1)) `Map.union` toBoard' (r, c + 1)

-- toList :: Board -> [[Number]]
-- toList board = toList' 1
--   where
--     toList' n
--       | n > boardSize = []
--       | otherwise = Map.elems (Map.filterWithKey (\(r, _) _ -> r == n) board) : toList' (n + 1)

-- main :: IO ()
-- main = do
--   nums <- forM [1..boardSize] (\_ -> map read . words <$> getLine)
--   let boards = map toList $ solve $ toBoard nums
--   mapM_ (mapM_ (putStrLn . intersperse ' ' . concat . map show)) boards 
  

-- test :: Board
-- test = toBoard testNums
--   where
--     testNums = [ [0, 0, 1, 2]
--                , [1, 2, 0, 0]
--                , [2, 1, 0, 0]
--                , [0, 0, 2, 1]
--                ]



-------------------


data SubID = Row Int
           | Col Int
           | Box Int
           deriving Eq

-- type Posp = (SubID, SubID) 


getSub :: Board -> SubID -> SubBoard
getSub b (Row n) = Map.filterWithKey (\(r, _) _ -> r == n) b
getSub b (Col n) = Map.filterWithKey (\(_, c) _ -> c == n) b
getSub b n@(Box _) = Map.filterWithKey (\p _ -> getBoxID p == n) b

getRowID, getColID, getBoxID :: Pos -> SubID
getRowID (r, _) = Row r 
getColID (_, c) = Col c
getBoxID (r, c) = Box $ r' * boxSize + c'
  where
    r' = (r - 1) `div` boxSize   -- 0 1 2
    c' = (c - 1) `div` boxSize + 1 -- 1 2 3

genPoses :: [Pos]
genPoses = genPoses' (1, 1)
  where
    genPoses' (r, c)
      | r > boardSize = []
      | c > boardSize = genPoses' (r + 1, 1)
      | otherwise = (r, c) : genPoses' (r, c + 1)

roundOne :: Board -> Board
roundOne b = foldl' entry b genPoses

roundEnough :: Board -> Board
roundEnough b = if b == b' then b else roundEnough b'
  where
    b' = roundOne b 

entry :: Board -> Pos -> Board
entry b p = if isDecided b p
              then b
              else Map.adjust (filter (`notElem` decidedNums)) p b 
  where
    decidedNums = concat . Map.elems . Map.filter (\a -> length a == 1) . Map.delete p $ getRCB b p

getRCB :: Board -> Pos -> SubBoard
getRCB b p = Map.unions [ getSub b (getRowID p)
                        , getSub b (getColID p)
                        , getSub b (getBoxID p)
                        ]

-- 確定かどうか判定する函数
isDecided :: Board -> Pos -> Bool
isDecided b p = (length . fromMaybe [] $ Map.lookup p b) == 1


-- 数独を最後まで解く。考えられるすべての解をだす。
solve :: Board -> [Board]
solve b 
  | finish b = return b
  | otherwise = update b >>= solve

-- 終了(全部埋まっているかどうか)かどうか判断
finish :: Board -> Bool
finish b = all (isDecided b) genPoses 

isFail :: Board -> Bool
isFail b = elem [] $ Map.elems b

-- 十分に入らない数字を消していってから、入る数字の数が少ないものを仮定
update :: Board -> [Board]
update b 
  | finish b' = return b'
  | isFail b' = []
  | otherwise = do
      n <- ns 
      return $ Map.adjust (const [n]) p b' 
  where
    b' = roundEnough b
    p = minLenPos b'
    ns = fromMaybe [] $ Map.lookup p b'
    

-- 一番入る数字が少ないものの位置(どれか一つだけ)
minLenPos :: Board -> Pos
minLenPos b = head . Map.keys $ Map.filter (== ns) b
  where
    ns = minimumBy (\a b -> compare (length a) (length b)) $ filter (\x -> length x /= 1) $ Map.elems b













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

toBoard :: [[Number]] -> Board 
toBoard nums = toBoard' (1, 1)
  where
    toBoard' (r, c) 
      | r > boardSize = Map.empty
      | c > boardSize = toBoard' (r + 1, 1)
      | otherwise = Map.singleton (r, c) (convert (nums !! (r - 1) !! (c - 1))) `Map.union` toBoard' (r, c + 1)
    convert n = if n == 0 
                  then [1..boardSize]
                  else [n]