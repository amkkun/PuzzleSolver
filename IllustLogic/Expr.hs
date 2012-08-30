{-# OPTIONS -Wall #-}

module Expr
       ( Expr(..)
       , toCnf
       , allAnd
       , allOr
       ) where

--

-- factorial :: Int -> Int
-- factorial n = product [1..n]

-- permutation :: Int -> Int -> Int
-- permutation n r = product [(n - r + 1)..n]

-- combination :: Int -> Int -> Int
-- combination n r = permutation n r `div` factorial r

--

data Expr a = Var a
            | Not (Expr a)
            | And (Expr a) (Expr a)
            | Or  (Expr a) (Expr a)
            deriving (Show, Eq)

toCnf :: (Eq a) => Expr a -> Expr a
toCnf = distribute . insideNot

-- Notを中にいれる
insideNot :: Expr a -> Expr a 
insideNot (Not e) = case e of
    (Not x) -> x
    (And x y) -> Or (insideNot (Not x)) (insideNot (Not y))
    (Or x y) -> And (insideNot (Not x)) (insideNot (Not y))
    x -> Not x
insideNot (And e1 e2) = And (insideNot e1) (insideNot e2)
insideNot (Or e1 e2) = Or (insideNot e1) (insideNot e2)
insideNot e = e

-- 分配法則でOrを中に。 insideNotの後。
distribute :: (Eq a) => Expr a -> Expr a 
distribute (And e1 e2) = And (distribute e1) (distribute e2)
distribute (Or e1 e2) = case (e1, e2) of
    ((And x y), z) -> And (distribute (Or x z)) (distribute (Or y z))
    (x, (And y z)) -> And (distribute (Or x y)) (distribute (Or x z))
    (x, y) -> let tmp = (Or (distribute x) (distribute y)) in -- 十分にOrが中に行くまで待つ。
        if (Or x y) == tmp 
            then tmp
            else distribute tmp   
distribute e = e



--

allAnd :: [Expr a] -> Expr a
allAnd [] = error "allAnd: argument is empty"
allAnd es = foldr And (head es) (tail es) -- foldlにしたらすごく遅かった

allOr :: [Expr a] -> Expr a
allOr [] = error "allOr: argument is empty"
allOr es = foldr Or (head es) (tail es)

-- ex2 :: Expr String
-- ex2 = Or (Or (And (Var "a") (Var "b")) (Var "c")) (Or (Var "d") (Var "f"))


