{-# OPTIONS -Wall #-}

{-
盤面をどう表現するか

-}

import Data.List
import Data.Maybe
import Control.Applicative
import Control.Monad

-- define type

type Board = [PosNums] 
type PosNums = (Pos, Nums) -- data PosNums = PosNums Pos Nums
type Pos = (Int, Int) -- data Pos = Pos Int Int
type Nums = [Int]

-- get-function

getPos :: PosNums -> Pos
getPos = fst

getNums :: PosNums -> Nums
getNums = snd

getX :: Pos -> Int
getX = fst

getY :: Pos -> Int
getY = snd

-- const

size :: Int
size = 3

doubleSize :: Int
doubleSize = size * size

--

reload :: Board -> Board
reload = reload' 1 1 
  where
    reload' x y board
      | y > doubleSize = if isFinish board then board else reload board
      | x > doubleSize = reload' 1 (succ y) board
      | (length <$> lookup (x, y) board) == Just 1 = reload' (succ x) y $ remove (x, y) board
      | otherwise = reload' (succ x) y board

isFinish :: Board -> Bool
isFinish = and . map (== 1) . map length . map getNums

remove :: Pos -> Board -> Board
remove p b = box (getX basePos) (getY basePos) . vertical 1 $ horizontal 1 b 
  where
    basePos = ((getX p - 1) `div` size * size + 1, (getY p - 1) `div` size * size + 1)
    num = fromMaybe 0 (head <$> lookup p b)
    box x y board
      | y == getY basePos + size = board
      | x == getX basePos + size = box (getX basePos) (succ y) board
      | (x, y) == p = box (succ x) y board
      | otherwise = box (succ x) y $ removeNum (x, y) num board
    vertical i board
      | i > doubleSize = board
      | i == getX p = vertical (succ i) board
      | otherwise = vertical (succ i) $ removeNum (i, getY p) num board
    horizontal i board
      | i > doubleSize = board
      | i == getY p = horizontal (succ i) board
      | otherwise = horizontal (succ i) $ removeNum (getX p, i) num board

removeNum :: Pos -> Int -> Board -> Board -- PosからIntを取り除く
removeNum p n b = (p, fromJust $ delete n <$> lookup p b) : filter (\x -> getPos x /= p) b

setBoard :: [[Int]] -> Board
setBoard = setBoard' 1 1 
  where
    setBoard' :: Int -> Int -> [[Int]] -> Board
    setBoard' _ _ [] = []
    setBoard' _ y ([]:xss) = setBoard' 1 (succ y) xss
    setBoard' x y ((n:ns):nss) 
      | n == 0 = ((x, y), [1..doubleSize]) : (setBoard' (succ x) y (ns:nss))
      | otherwise = ((x, y), [n]) : (setBoard' (succ x) y (ns:nss))

sortBoard :: PosNums -> PosNums -> Ordering
sortBoard a b
  | getY (getPos a) > getY (getPos b) = GT
  | getY (getPos a) == getY (getPos b) = compare (getX (getPos a)) (getX (getPos b))
  | otherwise = LT

displayBoard :: Board -> IO ()
displayBoard [] = return ()
displayBoard b = do
    putStr $ (show . head . getNums $ head b) ++ " " 
    if (getX $ getPos $ head b) == doubleSize 
    then do 
      putStrLn ""
      displayBoard $ tail b
    else 
      displayBoard $ tail b

main :: IO ()
main = do
    -- s <- read <$> getLine
    nums <- forM [1..doubleSize] (\_ -> map read . words <$> getLine)
    let board = sortBy sortBoard . reload $ setBoard nums
    putStrLn ""
    displayBoard board