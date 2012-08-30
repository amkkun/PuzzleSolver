{-# OPTIONS -Wall #-}

import Data.List (intersperse)
-- import Criterion.Main

-- const value
boardsize :: Int
boardsize = 9
boxsize :: Int
boxsize = 3 
cellvals :: Choices
cellvals = "123456789"
blank :: Char -> Bool
blank = (== '.') 

-- type declare
type Matrix a = [[a]]
type Board = Matrix Char

-- The function correct tests whether a filled board.
correct :: Board -> Bool
correct b = all nodups (rows b) && 
            all nodups (cols b) && 
            all nodups (boxs b)

nodups :: Eq a => [a] -> Bool
nodups [] = True
nodups (x:xs) = notElem x xs && nodups xs

-- rows, cols, boxs (a . a = id)
rows :: Matrix a -> Matrix a
rows = id

cols :: Matrix a -> Matrix a
cols [xs] = [[x] | x <- xs]
cols (xs:xss) = zipWith (:) xs (cols xss)

boxs :: Matrix a -> Matrix a
boxs = map ungroup . ungroup . map cols . group . map group

-- group, ungroup
group :: [a] -> [[a]]
group = groupBy boxsize
  where
    groupBy _ [] = []
    groupBy n xs = take n xs : groupBy n (drop n xs)


ungroup :: [[a]] -> [a]
ungroup = concat


type Choices = [Char]

choices :: Board -> Matrix Choices
choices = map $ map choose
  where
    choose e = if blank e then cellvals else [e]

mcp :: Matrix [a] -> [Matrix a]
mcp = cp . map cp

cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs:xss) = [x : ys | x <- xs, ys <- cp xss]

--------

fixed :: [Choices] -> Choices
fixed = concat . filter single

single :: [a] -> Bool
single = (==) 1 . length
    
reduce :: [Choices] -> [Choices]
reduce css = map (remove (fixed css)) css
  where
    remove fs cs = if single cs then cs else delete fs cs

delete :: Eq a => [a] -> [a] -> [a]
delete xs = filter (`notElem` xs)

-- これで十分なの？
prune :: Matrix Choices -> Matrix Choices
prune = pruneBy boxs . pruneBy cols . pruneBy rows 
-- prune cm
--   | cm == cm' = cm
--   | otherwise = prune cm 
--   where
--     cm' = pruneBy boxs . pruneBy cols . pruneBy rows $ cm

-- 変換してreduceして戻す
pruneBy :: (Matrix Choices -> Matrix Choices) -> 
           Matrix Choices -> Matrix Choices
pruneBy f = f . map reduce . f 

--------

-- 一番選べる数が少ないものを展開
expand :: Matrix Choices -> [Matrix Choices]
expand cm = [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
  where
    (rows1, row : rows2) = break (any best) cm -- [[]] ++ [best choice row] ++ [[]]
    (row1, cs : row2) = break best row -- [Char] ++ [best choice] ++ [Char]
    best cs = length cs == n -- :: [a] -> Bool
    n = minchoice cm -- :: Int

-- 入る可能性があるものの長さが最も小さいもの
minchoice :: Matrix [a] -> Int
minchoice = minimum . filter (> 1) . concat . map (map length) 

-- これ以上続けられないならブロック（True）
blocked :: Matrix Choices -> Bool
blocked cm = void cm || not (safe cm)

-- 一つでもnullがあったらTrue
void :: Matrix Choices -> Bool
void = any (any null) 

-- row, col, boxで確定された数字がかぶってなかったらTrue
safe :: Matrix Choices -> Bool
-- safe cm = all (nodups . fixed) (rows cm) &&
--           all (nodups . fixed) (cols cm) &&
--           all (nodups . fixed) (boxs cm) 
safe cm = and $ map (all (nodups . fixed)) [rows cm, cols cm, boxs cm] 

-- 
search :: Matrix Choices -> [Matrix Choices]
search cm  
  | blocked cm = [] -- 失敗
  | all (all single) cm = [cm] -- 成功
  | otherwise = expand cm >>= search . prune -- (concat . map (search . prune) . expand) cm

sudoku :: Board -> [Board]
sudoku = map (map concat) . search . prune . choices

--------


display :: Board -> IO ()
display = mapM_ putStrLn . map (intersperse ' ')

solve :: Board -> IO ()
solve = mapM_ display . sudoku

mat :: Board
mat = [ "8........"
      , "..36....."
      , ".7..9.2.."
      , ".5...7..."
      , "....457.."
      , "...1...3."
      , "..1....68"
      , "..85...1."
      , ".9....4.."
      ]

matbroken :: Board
matbroken = [ "8........"
           , "..36....."
           , ".7..9.2.."
           , ".5...7..."
           , "....457.."
           , "...1...3."
           , "..1....68"
           , "...5...1."
           , ".9....4.."
           ]
           
mat1 :: Board             
mat1 = [ "25..3..46"
       , "..9.2...8"
       , "43.761..9"
       , "...6....."
       , "1..984..5"
       , ".....2..."
       , "3..148.72"
       , "8...7.9.."
       , "74..9..53"
       ]

mat2 :: Board
mat2 = [ ".......1."
       , "4........"
       , ".2......."
       , "....5.4.7"
       , "..8...3.."
       , "..1.9...."
       , "3..4..2.."
       , ".5.1....."
       , "...8.6..."
       ]

mat3 :: Board
mat3 = [ ".......12"
       , "3......6."
       , "....4...."
       , "9.....5.."
       , ".....1.7."
       , ".2......."
       , "...35.4.."
       , "..14..8.."
       , ".6......."
       ]
       
main :: IO ()
main = do
  solve mat
     -- defaultMain [
     --   bgroup "sudoku" [ bench "1"   $ nf sudoku mat1
     --                   -- , bench "descending"  $ nf sort des
     --                   -- , bench "randoms"     $ nf sort ram
     --                   ]
     --   ]

