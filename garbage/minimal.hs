{-# OPTIONS -Wall #-}

-- 変数の名前からSATソルバで読み込める数字にする関数
-- 変数の個数
-- clauseの個数

-- import System.IO
-- import Data.Array
import Control.Applicative
import Control.Monad

-- 

subGridSize :: Int
subGridSize = 3

gridSize :: Int
gridSize = subGridSize * subGridSize

--
--

factorial :: Int -> Int
factorial n = product [1..n]

permutation :: Int -> Int -> Int
permutation n r = product [(n - r + 1)..n]

combination :: Int -> Int -> Int
combination n r = permutation n r `div` factorial r

--


data Variable = Variable (Int, Int, Int) deriving (Show, Eq, Ord)
data Literal = Literal Variable | Not Variable deriving (Show)
data Clause = Empty | Only Literal | Or Clause Clause deriving (Show)
-- data CNF = Empty | Only Clause | And CNF CNF deriving (Show) 

var2n :: Variable -> Int
var2n (Variable (r, c, n)) = (r - 1) * gridSize + c + (n - 1) * gridSize * gridSize

lite :: Literal -> Int
lite (Literal x) = var2n x
lite (Not x) = negate $ var2n x

encode :: Clause -> String
encode = encode'' . encode' 
  
encode' :: Clause -> [Int]
encode' Empty = []
encode' (Only x) = [lite x]
encode' (Or x y) = encode' x ++ encode' y

encode'' :: [Int] -> String
encode'' = foldr (\x -> ((show x ++ " ") ++)) "0\n"

makeClause :: [Literal] -> Clause
makeClause [] = Empty
makeClause (x:[]) = Only x
makeClause (x:xs) = Or (Only x) (makeClause xs) 

--

sample :: String
sample = concat . map encode $ entry ++ row ++ column ++ subGrid

-- make :: ((Int, Int) -> [Clause]) -> [Clause]
-- make f = concat $ map f [(x, y) | x <- nums, y <- nums]

nums :: [Int]
nums = [1..gridSize]

row :: [Clause]
row = map makeClause [[Not $ Variable (x, y, z), Not $ Variable (i, y, z)] | y <- nums, z <- nums, x <- init nums, i <- nums, x < i]

column :: [Clause]
column = map makeClause [[Not $ Variable (x, y, z), Not $ Variable (x, i, z)] | x <- nums, z <- nums, y <- init nums, i <- nums, y < i]

subGrid :: [Clause]
subGrid = map makeClause $ [
    [Not $ Variable ((subGridSize * i + x), (subGridSize * j + y), z), Not $ Variable ((subGridSize * i + x), (subGridSize * j + k), z)] 
    | z <- nums, i <- subNums', j <- subNums', x <- subNums', y <- subNums', k <- subNums', y < k
    ] ++ [
    [Not $ Variable ((subGridSize * i + x), (subGridSize * j + y), z), Not $ Variable ((subGridSize * i + k), (subGridSize * j + l), z)] 
    | z <- nums, i <- subNums', j <- subNums', x <- subNums, y <- subNums, k <- subNums, l <- subNums, y < k]
  where 
    subNums = [1..subGridSize]
    subNums' = map ((-) 1) subNums
 
entry :: [Clause]
entry = map makeClause [[Literal $ Variable (r, c, n) | n <- nums] | r <- nums, c <- nums]

--

numVar :: Int
numVar = gridSize * gridSize * gridSize

numClause :: Int -> Int
numClause n = gridSize * gridSize + 3 * gridSize * (combination gridSize 2) * gridSize + n

first :: Int -> String
first n = "p cnf " ++ (show numVar) ++ " " ++ (show $ numClause n) ++ "\n"

-- 

board :: [(Int, Int, Int)] -> (Int, String)
board = foo (length, concat) . map encode . map makeClause . map (:[]) . map Literal . map Variable . filter (\(_, _, n) -> n /= 0)

main :: IO ()
main = do
    s <- forM [1..gridSize] (\x -> zip3 (repeat x) [1..] . map read . words <$> getLine)
    let (n, str) = board $ concat s
    writeFile "cnf.txt" $ concat [first n, sample, str]

foo :: (([a] -> b), ([a] -> c)) -> [a] -> (b, c)
foo (f, g) xs = (f xs, g xs)