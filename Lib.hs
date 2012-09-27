module Lib where

import Data.List

-- divide 3 [1..10] == [[1,2,3],[4,5,6],[7,8,9],[10]]
divide :: Int -> [a] -> [[a]] 
divide _ [] = []
divide n xs = left : divide n right
  where
    (left, right) = splitAt n xs
    
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
