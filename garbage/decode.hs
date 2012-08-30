{-# OPTIONS -Wall #-}

module Main where

import Control.Applicative

rowSize :: Int
rowSize = 10

data Cell = Blank | Fill deriving (Eq)

instance Show Cell where
  show Blank = " "
  show Fill = "#"
  
display :: [[Cell]] -> IO ()
display = mapM_ (putStrLn . concat . map show) 

ready :: [Int] -> [[Cell]]
ready = dist rowSize . map trans
  where
    trans n = if n > 0 then Fill else Blank 
                
dist :: Int -> [a] -> [[a]]
dist _ [] = []
dist n xs = let (left, right) = splitAt n xs in left : dist n right

main :: IO ()
main = do
  ans <- lines <$> readFile "answer"
  if head ans == "SAT"
    then display . ready . take 100 . map read . words $ ans !! 1
    else putStrLn "It is not satisfiable!"