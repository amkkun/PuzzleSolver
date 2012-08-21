{-# OPTIONS -Wall #-}

module Main where

import Data.List
import Control.Applicative
import Control.Monad

type Board = [(Pos, Int)] -- 変更が容易でアクセスも簡単なデータ構造
type Pos = (Int, Int) -- (row [1..9], col [1..9])

getRow, getCol, getBox :: Board -> Pos -> [Int]
getRow board pos = map snd $ filter (\entry -> fst entry /= pos && fst (fst entry) == fst pos) board
getCol board pos = map snd $ filter (\entry -> fst entry /= pos && snd (fst entry) == snd pos) board
getBox board pos = map snd $ filter (\entry -> inBox (fst entry) pos) board

-- いろいろ名前がおかしいから変更する
inBox :: Pos -> Pos -> Bool
inBox target pos = ina (fst target) (fst pos) && ina (snd target) (snd pos)
  where
    tmp a = 3 * ((a - 1) `div` 3)
    ina tr r = tmp r + 1 <= tr && tr <= tmp r + 3

-- notElemでいいじゃん
rowCheck, colCheck, boxCheck, check :: Board -> Pos -> Int -> Bool
-- 行のなかで同じ数字がないかどうかチェック
rowCheck board pos num = notElem num (getRow board pos) 
-- 列のなかで同じ数字がないかどうかチェック
colCheck board pos num = notElem num (getCol board pos) 
-- boxのなかで同じ数字がないかどうかチェック
boxCheck board pos num = notElem num (getBox board pos) 
-- 数字が当てはまるかどうかチェック
check board pos num = all id [f board pos num | f <- [boxCheck, rowCheck, colCheck]]

-- 空いている位置において1-9まで当てはまるかどうかををチェック。当てはまったら上に返して、やっていって、Falseになったら戻ってくる

-- step :: Board -> Pos -> [Board]
-- step board pos = do
--     num <- [1..9]
--     if check board pos num 
--         then do
--             let board' = reload board pos num
--             case nextPos board' pos of
--                 Just pos' -> step board' pos'
--                 Nothing -> board' :
--         else return ()

-- step :: Board -> Pos -> [Board]
-- step board pos = filter (check board pos) [1..9] >>= (\num -> 
--     let board' = reload board pos num in 
--         case nextPos board' pos of
--             Just pos' -> step board' pos'
--             Nothing -> board'
--     )

step :: Board -> Pos -> Board
step board pos = filter (check board pos) [1..9] >>= (\num -> 
    let board' = reload board pos num in 
        case nextPos board' pos of
            Just pos' -> step board' pos'
            Nothing -> board'
    )

reload :: Board -> Pos -> Int -> Board
reload board pos num = (pos, num) : (filter (\entry -> fst entry /= pos) board)
-- where forward :: 
nextPos :: Board -> Pos -> Maybe Pos
nextPos board pos = lookup pos board >>= (\n -> 
    if n == 0 
        then return pos 
        else nextPos board $ next pos
    )

next :: Pos -> Pos
next pos = if snd pos < 9 
    then (fst pos, succ $ snd pos)
    else (succ $ fst pos, 1)

-- solve :: Board -> [Board]
-- solve board = case nextPos board (1, 1) of
--     Just pos = step board pos
--     Nothing = [board]
solve :: Board -> Board
solve board = case nextPos board (1, 1) of
    Just pos -> step board pos
    Nothing -> board

display :: Board -> IO ()
display = foo . map snd . sortBy (\e1 e2 -> compare (fst e1) (fst e2))
  where
    foo :: [Int] -> IO ()
    foo [] = return ()
    foo xs = do
        putStrLn $ concat $ map ((++ " ") . show) $ take 9 xs
        foo (drop 9 xs)
        

main :: IO ()
main = do
    board <- concat <$> forM [1..9] (\x -> zip (zip (repeat x) [1..]) . map read . words <$> getLine) 
    display $ solve board