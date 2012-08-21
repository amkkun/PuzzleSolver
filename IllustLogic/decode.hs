{-# OPTIONS -Wall #-}

module Main where

import Control.Applicative

rowSize :: Int
rowSize = 5

data Cell = Blank | Fill deriving (Eq)

instance Show Cell where
  show Blank = " "
  show Fill = "#"
  
display :: [[Cell]] -> IO ()
display = mapM_ (putStrLn . concat . map show) 

ready :: [Int] -> [[Cell]]
ready = dist rowSize . map trans . filter (/= 0)
  where
    trans n = if n > 0 then Fill else Blank 
                
dist :: Int -> [a] -> [[a]]
dist _ [] = []
dist n xs = take n xs : dist n (drop n xs)
                                      
main :: IO ()
main = do
  ans <- lines <$> readFile "answer"
  if head ans == "SAT"
    then display . ready . map read . words $ ans !! 1
    else putStrLn "It is not satisfiable!"