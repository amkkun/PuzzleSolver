{-# OPTIONS -Wall #-}

module Main where

import Data.List
import Control.Applicative
import Setting


display :: [Int] -> IO ()
display [] = return ()
display xs = do
    putStrLn . concat . map (\x -> 
        if x > 9 
            then (++ " ") $ show x 
            else (++ "  ") $ show x
        ) $ take gridSize xs
    display $ drop gridSize xs

ready :: [Int] -> [Int]
ready = map snd 
      . sortBy (\x y -> compare (fst x) (fst y)) 
      . map extract 
      . filter (> 0) 

extract :: Int -> (Int, Int)
extract x = ( (x - 1) `mod` (gridSize * gridSize) + 1
            , (x - 1) `div` (gridSize * gridSize) + 1
            )

main :: IO ()
main = do
    ans <- lines <$> readFile "answer"
    if head ans == "SAT"
        then display . ready . map read . words $ ans !! 1
        else putStrLn "error : It is not satisfiable"



