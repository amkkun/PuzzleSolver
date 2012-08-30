{-# OPTIONS -Wall -Werror #-}

import System.IO

fact :: Int -> Int
fact n = product [1..n]

size :: Int
size = 2

cnf :: String
cnf = firstCnf
    ++ (concat $ [f i | i <- [1..(size ^ 2)], f <- flip map [1..(size ^ 2)] rowCnf])
    ++ (concat $ [f i | i <- [1..(size ^ 2)], f <- flip map [1..(size ^ 2)] colCnf])
    ++ (concat $ [f i | i <- [1..(size ^ 2)], f <- flip map [1..(size ^ 2)] boxCnf])
    ++ lastCnf

firstCnf :: String
firstCnf = "p cnf " ++ show (size ^ 6) ++ " " ++ show (fact (size ^ 2 - 1) * size ^ 2 * (size ^ 2 * 3) + size ^ 4) ++ "\n"

rowCnf :: Int -> Int -> String
rowCnf val row = concat $ map tupleToString $ tuple nums
  where
    nums = take (size ^ 2) [(size ^ 4 * (val - 1) + size ^ 2 * (row - 1) + 1)..]

colCnf :: Int -> Int -> String
colCnf val col = concat $ map tupleToString $ tuple nums
  where
    nums = take (size ^ 2) [(size ^ 4 * (val - 1) + col), (size ^ 4 * (val - 1) + col + size ^ 2)..]

boxCnf :: Int -> Int -> String
boxCnf val box = concat $ map tupleToString $ tuple nums
  where
    nums = [1..(size ^ 2)]

lastCnf :: String
lastCnf = concat $ map listToString $ map pos [1..(size ^ 4)]
  where
    pos n = take (size ^ 2) [n, (n + size ^ 4)..]

tuple :: [Int] -> [(Int, Int)]
tuple xs = [(-i, -j) | i <- xs, j <- xs, i < j]

tupleToString :: (Int, Int) -> String
tupleToString (i, j) = show i ++ " " ++ show j ++ " 0\n"

listToString :: [Int] -> String
listToString [] = "0\n"
listToString (x:xs) = show x ++ " " ++ listToString xs

main :: IO ()
main = do
    writeFile "cnf.txt" cnf
    