{-# OPTIONS -Wall #-}

module Main where

import Data.List.Zipper
import Data.Tuple
import Control.Applicative
-- import Control.Monad
import System.Environment

type Pos = (Int, Int) -- (row, col)
type Board = [(Pos, Int)]

--
-- Board Information 
--

boxSize, boardSize :: Int
boxSize = 3
boardSize = boxSize * boxSize

--
-- Check
--

-- BoardからPosが属する行、列、ブロックの数字のリストを返す（Pos を除く）
getRow, getCol, getBox :: Board -> Pos -> [Int]
getRow board pos = map snd $ filter (\entry -> fst entry /= pos && fst (fst entry) == fst pos) board
getCol board pos = map snd $ filter (\entry -> fst entry /= pos && snd (fst entry) == snd pos) board
getBox board pos = map snd $ filter (\entry -> inBox (fst entry) pos) board
  where
    edge a = boxSize * ((a - 1) `div` boxSize)
    inside t r = edge r + 1 <= t && t <= edge r + boxSize
    inBox t p = inside (fst t) (fst p) && inside (snd t) (snd p)
  
-- その数字が入れるかどうかチェック
check :: Board -> Pos -> Int -> Bool 
check board pos num = all id [notElem num $ f board pos | f <- [getRow, getCol, getBox]]


--
-- Solve
--

-- 空いているマスを探す。なかったらNothing
lookupEmpty :: Board -> Maybe Pos
lookupEmpty = lookup 0 . map swap

-- メイン
solve :: Board -> [Board] -- stop?
solve board = case lookupEmpty board of
    Just pos -> concat . map solve $ makeBoardList board pos
    Nothing -> [board]

-- Posのマスを、考えられる数字で埋めて返す
makeBoardList :: Board -> Pos -> [Board]
makeBoardList board pos = reload board pos <$> filter (check board pos) [1..boardSize] 

-- Board の Pos のところを Int に変えて Board を返す
reload :: Board -> Pos -> Int -> Board
reload board pos num = toList . replace (pos, num) . lookupz pos $ fromList board
-- reload board pos num = (pos, num) : (filter (\entry -> fst entry /= pos) board)

-- lookupのZipper版（返すものがMaybe bではないけど）
lookupz :: (Eq a) => a -> Zipper (a, b) -> Zipper (a, b)
lookupz a z 
  | endp z = z
  | a == fst (cursor z) = z
  | otherwise = lookupz a $ right z


--
-- IO 
--

-- Boardを表示
displayOne :: Board -> IO ()
displayOne = displayOne' . map snd
  where
    displayOne' :: [Int] -> IO ()
    displayOne' [] = return ()
    displayOne' xs = do
        putStrLn $ concat $ map ((++ " ") . show) $ take boardSize xs
        displayOne' $ drop boardSize xs

-- Boardのリストを表示
display :: [Board] -> IO ()
display [] = return ()
display (board:boards) = do
    putStrLn ""
    displayOne board
    display boards

-- 標準入力から数字を受け取って、標準出力に解いたものを表示
main :: IO ()
main = do
    sudoku <- lines <$> (readFile =<< head <$> getArgs) 
    -- sudoku' <- forM [1.. gridSize] (\x -> 
    --     zip3 [1..] (repeat x) . map read . words <$> getLine)
    let board = [1.. boardSize] >>= (\x -> zip (zip (repeat x) [1..]) . map read . words $ sudoku !! (x - 1))             
    -- board <- concat <$> forM [1..boardSize] (\x -> zip (zip (repeat x) [1..]) . map read . words <$> getLine) 
        boards = solve board
    display boards
    putStrLn $ "solve : " ++ (show $ length boards)



--
-- Test 
--

test :: Board
test = [((1,1),3),((1,2),0),((1,3),0),((1,4),0)
       ,((2,1),0),((2,2),1),((2,3),4),((2,4),0)
       ,((3,1),0),((3,2),3),((3,3),2),((3,4),0)
       ,((4,1),0),((4,2),0),((4,3),0),((4,4),4)
       ]

test2 :: Board
test2 = [((1,1),8),((1,2),0),((1,3),0),((1,4),0),((1,5),0),((1,6),0),((1,7),0),((1,8),0),((1,9),0),((2,1),0),((2,2),0),((2,3),3),((2,4),6),((2,5),0),((2,6),0),((2,7),0),((2,8),0),((2,9),0),((3,1),0),((3,2),7),((3,3),0),((3,4),0),((3,5),9),((3,6),0),((3,7),2),((3,8),0),((3,9),0),((4,1),0),((4,2),5),((4,3),0),((4,4),0),((4,5),0),((4,6),7),((4,7),0),((4,8),0),((4,9),0),((5,1),0),((5,2),0),((5,3),0),((5,4),0),((5,5),4),((5,6),5),((5,7),7),((5,8),0),((5,9),0),((6,1),0),((6,2),0),((6,3),0),((6,4),1),((6,5),0),((6,6),0),((6,7),0),((6,8),3),((6,9),0),((7,1),0),((7,2),0),((7,3),1),((7,4),0),((7,5),0),((7,6),0),((7,7),0),((7,8),6),((7,9),8),((8,1),0),((8,2),0),((8,3),8),((8,4),5),((8,5),0),((8,6),0),((8,7),0),((8,8),1),((8,9),0),((9,1),0),((9,2),9),((9,3),0),((9,4),0),((9,5),0),((9,6),0),((9,7),4),((9,8),0),((9,9),0)]

-- testSolve :: [Board]
-- testSolve = concat $ map makeBoardList $ concat $ map makeBoardList $ concat $ map makeBoardList $ concat $ map makeBoardList $ concat $ map makeBoardList $ concat $ map makeBoardList $ concat $ map makeBoardList $ concat $ map makeBoardList $ concat $ map makeBoardList $ concat $ map makeBoardList $ concat $ map makeBoardList $ makeBoardList test

