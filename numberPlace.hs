{-# OPTIONS -Wall #-}

import Data.Array

--

size :: Int
size = areaSize * areaSize

areaSize :: Int
areaSize = 3

--

type Board = Array (Int, Int) Int

board :: [((Int, Int), Int)] -> Board
board = array ((1, 1), (size, size)) 



next :: Board -> (Int, Int) -> Board
next b (r, c) = next


canPut :: Board -> (Int, Int) -> [Int]
canPut b (r, c) = filter (check b (r, c)) [1..size]

--

check :: Board -> (Int, Int) -> Int -> Bool
check b (r, c) n = all id [areaCheck b (r, c) n, rowCheck b (r, c) n, colCheck b (r, c) n]


areaCheck :: Board -> (Int, Int) -> Int -> Bool
areaCheck b (r, c) n = all (/= n) $ getArea b (r, c)

rowCheck :: Board -> (Int, Int) -> Int -> Bool
rowCheck b (r, c) n = all (/= n) $ getRow b (r, c)

colCheck :: Board -> (Int, Int) -> Int -> Bool
colCheck b (r, c) n = all (/= n) $ getCol b (r, c)


getArea :: Board -> (Int, Int) -> [((Int, Int), Int)]
getArea b (r, c) = lift b [(! (i, j)) | i <- [((r - 1) `div` areaSize * areaSize + 1)..((r - 1) `div` areaSize * areaSize + areaSize)]
                                      , j <- [((c - 1) `div` areaSize * areaSize + 1)..((c - 1) `div` areaSize * areaSize + areaSize)]
                                      , (i, j) /= (r, c)
                                      ]

getRow :: Board -> (Int, Int) -> [((Int, Int), Int)]
getRow b (r, c) = lift b [(! (r, i)) | i <- [1..size], i /= c]

getCol :: Board -> (Int, Int) -> [((Int, Int), Int)]
getRow b (r, c) = lift b [(! (i, c)) | i <- [1..size], i /= r]

lift :: a -> [(a -> b)] -> [b]
lift _ [] = []
lift a (f:fs) = f a : lift a fs