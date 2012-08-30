{-# OPTIONS -Wall #-}

import Data.List
import Control.Applicative
import Control.Monad

type Matrix a = [[a]]

solve :: Matrix Int -> [Matrix Int]
solve matrix 
  | isFailed matrix = []
  | isCompleted matrix = [matrix]
  | otherwise = update matrix >>= solve
                
isFailed :: Matrix Int -> Bool                
isFailed matrix = any (any duplicate) $ map (map (filter (/= 0))) 
                  [matrix, transpose matrix, boxs matrix]
                  
boxs :: Matrix a -> Matrix a
boxs = map concat . concat . map (divide 3) . transpose . map (divide 3)

divide :: Int -> [a] -> [[a]]
divide _ [] = []
divide n xs = let (first, rest) = splitAt n xs in first : divide n rest    

duplicate :: Eq a => [a] -> Bool
duplicate [] = False
duplicate (x:xs) = elem x xs || duplicate xs
  
isCompleted :: Matrix Int -> Bool
isCompleted = all (all (/= 0))
  
update :: Matrix Int -> [Matrix Int]
update matrix = [left ++ [left' ++ [val] ++ right'] ++ right | val <- [1..9]]
  where
    (left, fit:right) = break (any (== 0)) matrix
    (left', _:right') = break (== 0) fit     
    
main :: IO ()
main = do
  sudoku <- forM [1..9] (\_ -> map read . words <$> getLine) 
  let answers = solve sudoku
  mapM_ (mapM_ (putStrLn . concat . intersperse " " . map show)) answers
