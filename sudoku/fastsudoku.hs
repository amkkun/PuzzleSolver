{-# OPTIONS -Wall #-}

import Data.Maybe
import Data.List (minimumBy, intersperse, sortBy)
import Control.Monad
import Control.Applicative
import qualified Data.Foldable as F
import Data.Sequence (Seq, (|>), (<|), (><))
import qualified Data.Sequence as S

-- newtype MonadPair m k a = MonadPair { runPair :: m (k, m a) }

type Pos = (Int, Int)
type CellPos = (Pos, Pos)

type Value = Int
type SeqMap k a = Seq (k, Seq a)
type PosVal = SeqMap CellPos Value
type ValPos = SeqMap Value CellPos

type SeqMatrix a = Seq (Seq a)
data Sudoku = Sudoku (Int, Int) (SeqMatrix Value)



-- replace (fromList [('a',[1,2,3]),('b',[1,3,5])]) = fromList [(1,['a','b']),(2,['a']),(3, 
replace :: Eq b => SeqMap a b -> SeqMap b a -- slow
replace s = do
  b <- seqNub $ F.foldMap snd s
  let as = liftM fst $ S.filter (seqElem b . snd) s
  return (b, as)
  
seqElem :: Eq a => a -> Seq a -> Bool
seqElem a = isJust . S.elemIndexL a

seqNub :: Eq a => Seq a -> Seq a
seqNub s
  | S.null s = S.empty
  | otherwise = head' <| seqNub (S.filter (/= head') s)
  where
    head' = S.index s 0

-- * one
elimOne :: PosVal -> PosVal
elimOne = elimOne' boxsPV . elimOne' colsPV . elimOne' rowsPV

elimOne' :: (PosVal -> Seq PosVal) -> PosVal -> PosVal
elimOne' f = join . liftM reduce . f

fixed :: Seq (Seq a) -> Seq a
fixed = join . S.filter single

isFixedPV :: (a, Seq b) -> Bool
isFixedPV = single . snd

single :: Seq a -> Bool
single = (==) 1 . S.length

remove :: Eq a => Seq a -> Seq a -> Seq a
remove xs ys
  | single xs = xs
  | otherwise = S.filter (not . (`seqElem` ys)) xs

reduce :: PosVal -> PosVal
reduce pv = liftM (\(p, vs) -> (p, remove vs fixedVals)) pv
  where
    fixedVals = join $ S.filter isFixedPV pv >>= return . snd

rowsPV :: PosVal -> Seq PosVal
rowsPV = groupBy (\(p1, _) (p2, _) -> isSameRow p1 p2)
  
colsPV :: PosVal -> Seq PosVal
colsPV = groupBy (\(p1, _) (p2, _) -> isSameCol p1 p2)

boxsPV :: PosVal -> Seq PosVal
boxsPV = groupBy (\(p1, _) (p2, _) -> isSameBox p1 p2)

isSameRow :: CellPos -> CellPos -> Bool
isSameRow p1 p2 = fst (fst p1) == fst (fst p2) &&
                  fst (snd p1) == fst (snd p2)

isSameCol :: CellPos -> CellPos -> Bool
isSameCol p1 p2 = snd (fst p1) == snd (fst p2) &&
                  snd (snd p1) == snd (snd p2)

isSameBox :: CellPos -> CellPos -> Bool
isSameBox p1 p2 = fst p1 == fst p2


groupBy :: Eq a => (a -> a -> Bool) -> Seq a -> Seq (Seq a)
groupBy f xs
  | S.null xs = S.empty
  | otherwise = fit <| groupBy f rest
  where
    (fit, rest) = S.partition (f $ S.index xs 0) xs



eliminate :: PosVal -> PosVal
eliminate = elimOne

mainSolve :: PosVal -> Seq PosVal
mainSolve pv
  | isFailed pv = S.empty
  | isCompleted pv = return pv
  | otherwise = update pv >>= mainSolve . eliminate 
                
isCompleted :: PosVal -> Bool                
isCompleted = F.all (\(_, vs) -> single vs)

isFailed :: PosVal -> Bool
isFailed pv = valErr pv || lenErr pv || F.any (\(_, vs) -> S.null vs) pv

lenErr :: PosVal -> Bool
lenErr pv = S.length pv /= boardsize * boardsize 
  where
    boardsize = maximum . F.toList $ F.foldMap snd pv

valErr :: PosVal -> Bool
valErr pv = F.or (fmap isDup (fmap takeSingle (rowsPV pv))) ||
            F.or (fmap isDup (fmap takeSingle (colsPV pv))) ||
            F.or (fmap isDup (fmap takeSingle (boxsPV pv)))

takeSingle :: SeqMap a b -> Seq (Seq b)
takeSingle = S.filter single . fmap snd

isDup :: Eq a => Seq a -> Bool
isDup s
  | S.null s = False
  | otherwise = seqElem x s || isDup (S.filter (/= x) s)
  where
    x = S.index s 0
    
update :: PosVal -> Seq PosVal
update pv = minpvs >>= \minpv -> return $ pv1 >< (minpv <| (S.drop 1 pv2))
  where
    comp xs ys = compare (S.length xs) (S.length ys)
    minchoice = minimumBy comp . F.toList . S.filter ((1 <) . S.length) $ liftM snd pv
    (pv1, pv2) = S.breakl (\(_, vs) -> vs == minchoice) pv
    minpvs = minchoice >>= \x -> return (fst $ S.index pv2 0 , S.singleton x)
    
solve :: Sudoku -> Seq (SeqMatrix Value)
solve = fmap toMatrix . mainSolve . eliminate . analyze

analyze :: Sudoku -> PosVal
analyze (Sudoku (row, col) matrix) = 
  S.zip (cellposList row col) (vals matrix)
  where
    vals = liftM (\x -> if x == 0 then S.fromList [1..(row * col)] else S.singleton x) . join

cellposList :: Int -> Int -> Seq CellPos
cellposList rsize csize = cps (1,1) (1,1) 
  where
    cps (br, bc) (r, c)
      | br > csize = S.empty
      | r > rsize = cps (br + 1, 1) (1, 1)
      | bc > rsize = cps (br, 1) (r + 1, 1)
      | c > csize = cps (br, bc + 1) (r, 1)
      | otherwise = ((br, bc), (r, c)) <| cps (br, bc) (r, c + 1)

toMatrix :: PosVal -> SeqMatrix Value
toMatrix pv = divide boardsize . join . fmap snd $ sorted
  where
    boardsize = intSqrt (S.length pv)
    sorted = S.fromList $ sortBy (\(p1, _) (p2, _) -> compare (replace p1) (replace p2)) $ F.toList pv
    replace ((a, b), (c, d)) = ((a, c), (b, d))

intSqrt :: Int -> Int
intSqrt num = intSqrt' num 1
  where
    intSqrt' n m
      | n < m * m = m - 1
      | otherwise = intSqrt' n (m + 1)

divide :: Int -> Seq a -> Seq (Seq a)
divide n xs
  | S.null xs = S.empty
  | otherwise = left <| divide n right
  where
    (left, right) = S.splitAt n xs
    
display :: Show a => SeqMatrix a -> IO ()
display = F.mapM_ (putStrLn . concat . (intersperse " ") . F.toList . fmap show)

getSudoku :: IO Sudoku
getSudoku = do
  (rowsize:colsize:_) <- map read . words <$> getLine
  let boardsize = rowsize * colsize
  matrix <- liftM S.fromList $ forM [1..boardsize] $ \_ -> S.fromList . map read . words <$> getLine
  return $ Sudoku (rowsize, colsize) matrix
  
main :: IO ()
main = do
  -- files <- getArgs
  -- sudoku <- getSudoku
  -- let answers = solve sudoku
  -- mapM_ display answers
  -- putStrLn "--"
  -- sudoku17
  showSolve hard
  -- showSolve large
  
sudoku17 :: IO ()
sudoku17 = forever $ do
  sudoku <- divide 9 . S.fromList . map read . divideOne <$> getLine
  F.mapM_ display $ solve (Sudoku (3, 3) sudoku)
  putStrLn "--"
  where
    divideOne [] = []
    divideOne (x:xs) = [x] : divideOne xs
  
showSolve :: Sudoku -> IO ()
showSolve = F.mapM_ display . solve 

-- easy :: Sudoku
-- easy = (3, 3, [ [2,5,0,0,3,0,0,4,6]
--               , [0,0,9,0,2,0,0,0,8]
--               , [4,3,0,7,6,1,0,0,9]
--               , [0,0,0,6,0,0,0,0,0]
--               , [1,0,0,9,8,4,0,0,5]
--               , [0,0,0,0,0,2,0,0,0]
--               , [3,0,0,1,4,8,0,7,2]
--               , [8,0,0,0,7,0,9,0,0]
--               , [7,4,0,0,9,0,0,5,3]
--               ])


-- small :: Sudoku
-- small = (2, 2, [ [1,2,0,4]
--               , [0,0,0,2]
--               , [0,3,4,1]
--               , [4,1,2,3]
--               ])

hard :: Sudoku
hard = Sudoku (3, 3)
       $ S.fromList [ S.fromList [8,0,0,0,0,0,0,0,0]
                    , S.fromList [0,0,3,6,0,0,0,0,0]
                    , S.fromList [0,7,0,0,9,0,2,0,0]
                    , S.fromList [0,5,0,0,0,7,0,0,0]
                    , S.fromList [0,0,0,0,4,5,7,0,0]
                    , S.fromList [0,0,0,1,0,0,0,3,0]
                    , S.fromList [0,0,1,0,0,0,0,6,8]
                    , S.fromList [0,0,8,5,0,0,0,1,0]
                    , S.fromList [0,9,0,0,0,0,4,0,0]
                    ]
       
-- large :: Sudoku
-- large = (5, 5, [ [0,12,0,10,0,13,0,7,0,11,0,0,8,0,0,23,0,21,0,14,0,15,0,9,0]
--                , [0,22,11,25,0,1,0,15,0,18,0,0,6,0,0,8,0,12,0,4,0,5,21,17,0]
--                , [1,0,0,0,8,0,0,10,0,0,24,7,0,4,25,0,0,5,0,0,11,0,0,0,2]
--                , [18,0,7,0,6,0,22,0,2,0,0,5,0,9,0,0,13,0,16,0,20,0,10,0,24]
--                , [0,4,0,23,0,5,0,24,0,3,0,0,21,0,0,11,0,22,0,25,0,13,0,7,0]
--                , [19,0,0,0,1,0,14,11,24,0,15,0,0,0,16,0,8,9,13,0,4,0,0,0,6]
--                , [0,0,13,0,0,15,0,12,0,4,25,0,10,0,23,5,0,24,0,2,0,0,7,0,0]
--                , [11,23,0,9,20,21,5,0,18,22,0,14,0,7,0,15,19,0,25,6,17,12,0,3,8]
--                , [15,0,18,0,21,0,0,19,0,0,5,0,11,0,22,0,0,4,0,0,24,0,20,0,10]
--                , [0,25,0,24,17,0,10,0,3,0,21,0,0,0,6,0,20,0,11,0,19,1,0,5,0]
--                , [10,0,0,0,11,9,17,0,4,0,0,21,0,6,0,0,12,0,22,16,18,0,0,0,23]
--                , [0,14,0,18,0,10,0,1,0,5,17,0,20,0,3,9,0,25,0,23,0,11,0,6,0]
--                , [13,0,0,0,3,0,0,16,0,0,10,1,0,2,14,0,0,18,24,0,8,0,0,0,22]
--                , [0,5,12,6,0,11,0,18,0,8,0,0,16,0,0,10,0,2,0,19,0,3,14,20,0]
--                , [16,0,0,0,25,0,19,0,23,0,11,0,4,0,18,0,14,0,20,0,5,0,0,0,1]
--                , [0,21,1,14,0,24,0,0,0,17,0,10,9,5,0,6,0,0,0,13,0,19,18,2,0]
--                , [12,0,0,15,18,0,4,6,1,0,16,0,2,0,21,0,25,11,5,0,7,22,0,0,13]
--                , [0,10,19,17,0,16,0,0,0,12,7,18,0,1,13,2,0,0,0,15,0,24,11,21,0]
--                , [25,0,0,0,22,7,0,5,0,10,0,23,0,20,0,18,0,1,0,17,12,0,0,0,15]
--                , [0,24,0,16,0,0,3,0,8,0,4,0,22,0,15,0,7,0,12,0,0,17,0,1,0]
--                , [4,0,0,0,15,3,0,21,0,14,0,11,19,8,0,22,0,13,0,24,2,0,0,0,7]
--                , [8,19,0,20,10,0,16,0,5,0,23,0,0,0,9,0,2,0,14,0,22,18,0,11,3]
--                , [23,0,0,11,12,0,18,8,10,0,2,25,0,24,20,0,9,7,6,0,13,14,0,0,21]
--                , [0,7,22,3,0,2,25,0,11,24,0,0,15,0,0,4,18,0,19,20,0,10,1,8,0]
--                , [0,18,0,2,0,23,0,4,0,19,0,22,14,12,0,16,0,8,0,1,0,20,0,15,0]
--                ])

         

-- failed :: Sudoku
-- failed = Sudoku (2, 2) [ [1, 2, 3, 4]
--                 , [2, 3, 0, 0]
--                 , [0, 0, 0, 0]
--                 , [0, 0, 0, 0]
--                 ])


-- instance (Monad m) => Functor (MonadPair m k) where
--   fmap f (MonadPair m) = MonadPair $ do
--     (k, v) <- m
--     return $ (k, liftM f v) 

-- instance (Foldable m) => Foldable (MonadPair m k) where
--   foldMap f (MonadPair m) = foldMap (foldMap f . snd) m
    
-- -- instance Traversable (MonadPair m k) where
-- --   traverse :: (a -> [b]) -> MonadPair [] k a -> [MonadPair [] k b]
-- --   traverse f (MonadPair m) = fmap f  
  
-- --   sequenceA (MonadPair m) = (k, v) <- m
    
                     
-- newtype PairList k v = PairList [(k, [v])]                     
                     


-- test :: MonadPair [] Char Int
-- test = MonadPair [('a', [1..6]), ('b', [3..9])]
    
-- type Value = Int
-- type Pos = (Int, Int)

-- type ValPos = MonadPair [] Value Pos

-- -- replace . replace == id
-- replace :: Monad m => MonadPair m k v -> MonadPair m v k
-- replace (MonadPair m) = MonadPair $ do
--   (k, mv) <- m
--   v <- mv
--   return (v, return k)

