{-# OPTIONS -Wall #-}

module Sudoku where

-- import Control.Applicative
import Data.List
import Data.Maybe
import qualified Data.Map as M

data Sub = Row | Col | Box deriving (Eq, Show)

type Pos = (Int, Int) -- (Row, Col)

data Board a = Board { getSize :: Int
                     , getBoard :: M.Map Pos a
                     } deriving (Eq, Show)
--

blank :: Int -> Bool
blank = (== 0)

values :: Int -> [Int]
values n = [1..n]

boxSize :: Int -> Int
boxSize n = if n == 9 then 3 else 2

getSubs :: Board a -> Sub -> [Board a]
getSubs (Board s b) sub = do
  n <- values s 
  return $ Board s $ M.filterWithKey (\p _ -> getID p sub == n) b
  where
    bs = boxSize s
    getID (r, _) Row = r
    getID (_, c) Col = c
    getID (r, c) Box = r' * bs + c'
      where
        r' = (r - 1) `div` bs   -- 0 1 2
        c' = (c - 1) `div` bs + 1 -- 1 2 3      
                                                         
--

fixed :: Board [Int] -> [Int]
fixed (Board _ b) = concat . filter single $ M.elems b

single :: [a] -> Bool
single = (== 1) . length

prune :: Board [Int] -> Board [Int]
prune b = foldl' pruneBy b [Box, Row, Col]
    
pruneBy :: Board [Int] -> Sub -> Board [Int]
pruneBy b s = Board (getSize b) $ M.unions b'
  where
    remove fs cs = if single cs then cs else filter (`notElem` fs) cs
    b' = do
      sub <- getSubs b s
      let fs = fixed sub
      return $ M.map (remove fs) $ getBoard sub

choices :: Board Int -> Board [Int]
choices (Board s b) = Board s b'
  where
    b' = M.map (\n -> if blank n then [1..s] else [n]) b

settle :: Board [Int] -> Board Int
settle (Board s b) = Board s b'
  where
    b' = M.map (\n -> if single n then head n else 0) b
--
    
solve :: Board [Int] -> [Board [Int]]
solve b
  | isFailed b = []
  | isCompleted b = [b]
  | otherwise = update b >>= solve . prune

nobups :: Eq a => [a] -> Bool
nobups [] = True
nobups (x:xs) = notElem x xs && nobups xs 

safe :: Board [Int] -> Bool
safe = nobups . concat . filter single . M.elems . getBoard

isFailed :: Board [Int] -> Bool
isFailed b = void b || unsafe b
  where
    void = any null . M.elems . getBoard
    unsafe b = not . all safe . concat $ map (getSubs b) [Row, Col, Box] 
    
isCompleted :: Board [Int] -> Bool
isCompleted = null . filter (not . single) . M.elems . getBoard

update :: Board [Int] -> [Board [Int]]
update (Board s b) = [Board s $ M.adjust (const [n]) p b | n <- fromJust $ M.lookup p b] 
  where
    minchoice = minimum . filter (> 1) . map length $ M.elems b
    p = head . M.keys $ M.filter (\x -> length x == minchoice) b

sudoku :: Board Int -> IO ()
sudoku = mapM_ displayBoard . map settle . solve . prune . choices


--

setIndex2 :: [[a]] -> [((Int, Int), a)]
setIndex2 = concat . map (map swap3) . map (zip [1..]) . map divide . zip [1..]
  where
    divide (n, xs) = [(n, x) | x <- xs]
    swap3 (a, (b, c)) = ((b, a), c)

removeIndex2 :: [((Int, Int), a)] -> [[a]]
removeIndex2  = map (map snd) . groupRow . map snd . map swap3 
  where
    swap3 ((a, b), c) = (b, (a, c))
    groupRow = groupBy (\(r, _) (r', _) -> r == r')

toBoard :: [[a]] -> Board a
toBoard xss = Board size board 
  where
    board = M.fromList $ setIndex2 xss
    ((size, _), _) = M.findMax board 
                              
fromBoard :: Board a -> [[a]]
fromBoard  = removeIndex2 . M.toList . getBoard 

    
    
display2 :: (Show a) => [[a]] -> IO ()     
display2 = mapM_ (putStrLn . intersperse ' ' . concat . map show)

displayBoard :: (Show a) => Board a -> IO ()
displayBoard = display2 . fromBoard
    
    
test :: Board Int
test = toBoard testNums

testNums :: [[Int]]    
testNums = [ [2, 5, 0, 0, 3, 0, 0, 4, 6]
           , [0, 0, 9, 0, 2, 0, 0, 0, 8]
           , [4, 3, 0, 7, 6, 1, 0, 0, 9]
           , [0, 0, 0, 6, 0, 0, 0, 0, 0]
           , [1, 0, 0, 9, 8, 4, 0, 0, 5]
           , [0, 0, 0, 0, 0, 2, 0, 0, 0]
           , [3, 0, 0, 1, 4, 8, 0, 7, 2]
           , [8, 0, 0, 0, 7, 0, 9, 0, 0]
           , [7, 4, 0, 0, 9, 0, 0, 5, 3]
           ]
    

test3 :: Board Int
test3 = toBoard testNums
  where
    testNums = [ [8, 0, 0, 0, 0, 0, 0, 0, 0]
               , [0, 0, 3, 6, 0, 0, 0, 0, 0]
               , [0, 7, 0, 0, 9, 0, 2, 0, 0]
               , [0, 5, 0, 0, 0, 7, 0, 0, 0]
               , [0, 0, 0, 0, 4, 5, 7, 0, 0]
               , [0, 0, 0, 1, 0, 0, 0, 3, 0]
               , [0, 0, 1, 0, 0, 0, 0, 6, 8]
               , [0, 0, 8, 5, 0, 0, 0, 1, 0]
               , [0, 9, 0, 0, 0, 0, 4, 0, 0]
               ]