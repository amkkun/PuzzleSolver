
import Data.List

type Matrix a = [[a]]
type Board a = Matrix (Matrix a)
type Value = Int
type Sudoku = (Int, Int, Matrix Value)

rows :: Board a -> Board a
rows = map transpose
rowsBack :: Board a -> Board a
rowsBack = map transpose

cols :: Board a -> Board a
cols = transpose . map transpose . transpose . map transpose . map (map transpose)
colsBack :: Board a -> Board a
colsBack = map (map transpose) . map transpose . transpose . map transpose . transpose

single :: [a] -> Bool
single xs = length xs == 1

prune :: Eq a => Matrix [a] -> Matrix [a]
prune bunch = map (map (\xs -> if single xs then xs else filter (`notElem` fixed) xs)) bunch 
  where
    fixed = concat . filter single . concat $ bunch


eliminate :: Eq a => Board [a] -> Board [a]
eliminate = colsBack . map2 prune . cols . rowsBack . map2 prune . rows . map2 prune

eliminateRepeat :: Eq a => Board [a] -> Board [a]
eliminateRepeat b = if b == b' then b else eliminateRepeat b'
  where
    b' = eliminate b

divide :: Int -> [a] -> [[a]]
divide _ [] = []
divide n xs = first : divide n rest
  where
    (first, rest) = splitAt n xs

map2 :: (a -> b) -> [[a]] -> [[b]]
map2 f = map (map f)

concat2 :: [[[a]]] -> [a]
concat2 = concat . concat

solve :: Board [Value] -> [Matrix Value]
solve b
  | isFailed b = []
  | isCompleted b = return . map concat . concat . map (map concat . transpose) $ b 
  | otherwise = update b >>= solve . eliminate
                
isFailed :: Board [Value] -> Bool
isFailed b = any (any (any (any null))) b || varErr b

varErr :: Board [Value] -> Bool
varErr b = any (any isDup . map (filter single) . map concat . concat) [rows b, cols b, b]


isDup :: Eq a => [a] -> Bool
isDup [] = False
isDup (x:xs) = elem x xs || isDup xs
  
isCompleted :: Board [a] -> Bool  
isCompleted = all (all (all (all single)))

update :: Eq a => Board [a] -> [Board [a]]
update b = do
  choice <- choices
  return $ first ++ [first' ++ [first'' ++ [first''' ++ [[choice]] ++ rest'''] ++ rest''] ++ rest'] ++ rest
  where
    choices = minimumBy (\x y -> compare (length x) (length y)) . filter (not . single) . concat . concat2 $ b
    (first, fit:rest) = break (any (any (elem choices))) b
    (first', fit':rest') = break (any (elem choices)) fit
    (first'', fit'':rest'') = break (elem choices) fit'
    (first''', _:rest''') = break (== choices) fit''
    
analyze :: Sudoku -> Board [Value]
analyze (rsize, csize, matrix) = 
  rowsBack . map2 (divide csize) . divide csize . map2 choice $ matrix
  where                                 
    choice x = if x == 0 then [1..rsize * csize] else [x] -- :: Value -> [Value]

display :: Show a => Matrix a -> IO ()
display = mapM_ (putStrLn . concat . intersperse " " . map show)

main :: IO ()
main = do
  mapM_ display $ solve $ analyze hard

sudoku :: Sudoku -> IO ()
sudoku = mapM_ display . solve . analyze

hard :: Sudoku
hard = (3, 3, [ [8,0,0,0,0,0,0,0,0]
              , [0,0,3,6,0,0,0,0,0]
              , [0,7,0,0,9,0,2,0,0]
              , [0,5,0,0,0,7,0,0,0]
              , [0,0,0,0,4,5,7,0,0]
              , [0,0,0,1,0,0,0,3,0]
              , [0,0,1,0,0,0,0,6,8]
              , [0,0,8,5,0,0,0,1,0]
              , [0,9,0,0,0,0,4,0,0]
              ])


easy :: Sudoku
easy = (3, 3, [ [2,5,0,0,3,0,0,4,6]
              , [0,0,9,0,2,0,0,0,8]
              , [4,3,0,7,6,1,0,0,9]
              , [0,0,0,6,0,0,0,0,0]
              , [1,0,0,9,8,4,0,0,5]
              , [0,0,0,0,0,2,0,0,0]
              , [3,0,0,1,4,8,0,7,2]
              , [8,0,0,0,7,0,9,0,0]
              , [7,4,0,0,9,0,0,5,3]
              ])

small :: Sudoku
small = (2, 2, [ [1,2,3,4]
               , [3,4,1,2]
               , [2,3,4,1]
               , [4,1,2,3]
               ])

large :: Sudoku
large = (5, 5, [ [0,12,0,10,0,13,0,7,0,11,0,0,8,0,0,23,0,21,0,14,0,15,0,9,0]
               , [0,22,11,25,0,1,0,15,0,18,0,0,6,0,0,8,0,12,0,4,0,5,21,17,0]
               , [1,0,0,0,8,0,0,10,0,0,24,7,0,4,25,0,0,5,0,0,11,0,0,0,2]
               , [18,0,7,0,6,0,22,0,2,0,0,5,0,9,0,0,13,0,16,0,20,0,10,0,24]
               , [0,4,0,23,0,5,0,24,0,3,0,0,21,0,0,11,0,22,0,25,0,13,0,7,0]
               , [19,0,0,0,1,0,14,11,24,0,15,0,0,0,16,0,8,9,13,0,4,0,0,0,6]
               , [0,0,13,0,0,15,0,12,0,4,25,0,10,0,23,5,0,24,0,2,0,0,7,0,0]
               , [11,23,0,9,20,21,5,0,18,22,0,14,0,7,0,15,19,0,25,6,17,12,0,3,8]
               , [15,0,18,0,21,0,0,19,0,0,5,0,11,0,22,0,0,4,0,0,24,0,20,0,10]
               , [0,25,0,24,17,0,10,0,3,0,21,0,0,0,6,0,20,0,11,0,19,1,0,5,0]
               , [10,0,0,0,11,9,17,0,4,0,0,21,0,6,0,0,12,0,22,16,18,0,0,0,23]
               , [0,14,0,18,0,10,0,1,0,5,17,0,20,0,3,9,0,25,0,23,0,11,0,6,0]
               , [13,0,0,0,3,0,0,16,0,0,10,1,0,2,14,0,0,18,24,0,8,0,0,0,22]
               , [0,5,12,6,0,11,0,18,0,8,0,0,16,0,0,10,0,2,0,19,0,3,14,20,0]
               , [16,0,0,0,25,0,19,0,23,0,11,0,4,0,18,0,14,0,20,0,5,0,0,0,1]
               , [0,21,1,14,0,24,0,0,0,17,0,10,9,5,0,6,0,0,0,13,0,19,18,2,0]
               , [12,0,0,15,18,0,4,6,1,0,16,0,2,0,21,0,25,11,5,0,7,22,0,0,13]
               , [0,10,19,17,0,16,0,0,0,12,7,18,0,1,13,2,0,0,0,15,0,24,11,21,0]
               , [25,0,0,0,22,7,0,5,0,10,0,23,0,20,0,18,0,1,0,17,12,0,0,0,15]
               , [0,24,0,16,0,0,3,0,8,0,4,0,22,0,15,0,7,0,12,0,0,17,0,1,0]
               , [4,0,0,0,15,3,0,21,0,14,0,11,19,8,0,22,0,13,0,24,2,0,0,0,7]
               , [8,19,0,20,10,0,16,0,5,0,23,0,0,0,9,0,2,0,14,0,22,18,0,11,3]
               , [23,0,0,11,12,0,18,8,10,0,2,25,0,24,20,0,9,7,6,0,13,14,0,0,21]
               , [0,7,22,3,0,2,25,0,11,24,0,0,15,0,0,4,18,0,19,20,0,10,1,8,0]
               , [0,18,0,2,0,23,0,4,0,19,0,22,14,12,0,16,0,8,0,1,0,20,0,15,0]
               ])


tes :: Matrix Value
tes = [ [2,5,0,0,3,0,0,4,6]
      , [0,0,9,0,2,0,0,0,8]
      , [4,3,0,7,6,1,0,0,9]
      , [0,0,0,6,0,0,0,0,0]
      , [1,0,0,9,8,4,0,0,5]
      , [0,0,0,0,0,2,0,0,0]
      , [3,0,0,1,4,8,0,7,2]
      , [8,0,0,0,7,0,9,0,0]
      , [7,4,0,0,9,0,0,5,3]
      ]
    
test :: Board Value
test = [ [ [ [11, 12, 13]
           , [21, 22, 23]
           , [31, 32, 33]
           ]
         , [ [14, 15, 16]
           , [24, 25, 26]
           , [34, 35, 36]
           ]
         , [ [17, 18, 19]
           , [27, 28, 29]
           , [37, 38, 39]
           ]
         ]
       , [ [ [41, 42, 43]
           , [51, 52, 53]
           , [61, 62, 63]
           ]
         , [ [44, 45, 46]
           , [54, 55, 56]
           , [64, 65, 66]
           ]
         , [ [47, 48, 49]
           , [57, 58, 59]
           , [67, 68, 69]
           ]
         ]    
       , [ [ [71, 72, 73]
           , [81, 82, 83]
           , [91, 92, 93]
           ]
         , [ [74, 75, 76]
           , [84, 85, 86]
           , [94, 95, 96]
           ]
         , [ [77, 78, 79]
           , [87, 88, 89]
           , [97, 98, 99]
           ]
         ]
       ]
       

