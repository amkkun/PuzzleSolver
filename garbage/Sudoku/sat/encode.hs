{-# OPTIONS -Wall #-}

import Control.Applicative
-- import Control.Monad
import System.Environment
import Expression
import Setting
import Minimal
-- import Extended


toNum :: Expr (Int, Int, Int) -> Int -- var -> num
toNum (Var (x, y, z)) = x + (y - 1) * gridSize + (z - 1) * gridSize * gridSize
toNum (Not e) = negate $ toNum e
toNum _ = error "toNum Exception: It is NOT variable."

encode :: Expr (Int, Int, Int) -> String -- CNF 
encode (And e1 e2) = encode e1 ++ " 0\n" ++ encode e2
encode (Or e1 e2) = encode e1 ++ " " ++ encode e2
encode e = show $ toNum e

sat :: String
sat = encode . toCnf . allAnd $ [entry, row, column, subGrid]
-- sat = encode . removeEmpty . toCnf . allAnd $ [entry, row, column, subGrid]


-- はじめの一行
first :: Int -> String
first n = "p cnf " ++ (show numVar) ++ " " ++ (show $ numClause n) ++ "\n"

board :: [(Int, Int, Int)] -> String -- TODO name
board = encode . allAnd . map Var 

main :: IO ()
main = do
    sudoku <- lines <$> (readFile =<< head <$> getArgs) 
    -- sudoku' <- forM [1.. gridSize] (\x -> 
    --     zip3 [1..] (repeat x) . map read . words <$> getLine)
    let sudoku' = [ zip3 [1..] (repeat x) (map read . words $ sudoku !! (x - 1)) 
                  | x <- [1.. gridSize]
                  ]
        s = filter (\(_, _, z) -> z /= 0) $ concat sudoku'
        str = board s -- TODO name
        num = length s
    writeFile "question" $ concat [first num, sat, " 0\n", str, " 0"]
