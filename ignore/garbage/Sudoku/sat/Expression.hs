{-# OPTIONS -Wall #-}

module Expression 
       ( Expr
       , toCnf
       , allAnd
       , allOr
       ) where

--

factorial :: Int -> Int
factorial n = product [1..n]

permutation :: Int -> Int -> Int
permutation n r = product [(n - r + 1)..n]

combination :: Int -> Int -> Int
combination n r = permutation n r `div` factorial r

--


-- data Var = Var (Int, Int, Int) deriving (Show, Eq, Ord)
-- data Literal = Literal Var | Not Var deriving (Show)
-- data Clause = Empty | Only Literal | Or Clause Clause deriving (Show)


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
    (x, y) -> let tmp = (Or (distribute x) (distribute y)) in -- なんか深さ優先的なやつ。十分にOrが中に行くまで待つ。
        if (Or x y) == tmp 
            then tmp
            else distribute tmp   
distribute e = e

-- -- Emptyをなくす。再帰的に。
-- removeEmpty :: (Eq a) => Expr a -> Expr a
-- removeEmpty (And e1 e2) = case (e1, e2) of
--     (Empty, x) -> removeEmpty x
--     (x, Empty) -> removeEmpty x
--     (x, y) -> And (removeEmpty x) (removeEmpty y)
--     -- (x, y) -> let tmp = (And (removeEmpty x) (removeEmpty y)) in
--     --     if (And x y) == tmp 
--     --         then tmp
--     --         else removeEmpty tmp
-- removeEmpty (Or e1 e2) = case (e1, e2) of
--     (Empty, x) -> removeEmpty x
--     (x, Empty) -> removeEmpty x
--     (x, y) -> Or (removeEmpty x) (removeEmpty y)
--     -- (x, y) -> let tmp = (Or (removeEmpty x) (removeEmpty y)) in
--     --     if (Or x y) == tmp 
--     --         then tmp
--     --         else removeEmpty tmp     
-- removeEmpty (Not e) = case e of
--     Empty -> Empty
--     x -> Not $ removeEmpty x
--     -- x -> let tmp = removeEmpty x in
--     --     if x == tmp
--     --         then Not tmp
--     --         else Not $ removeEmpty tmp
-- removeEmpty e = e


--

allAnd :: [Expr a] -> Expr a
allAnd [] = error "allAnd: argument is empty"
allAnd es = foldr And (head es) (tail es) -- foldlにしたらすごく遅かった

allOr :: [Expr a] -> Expr a
allOr [] = error "allOr: argument is empty"
allOr es = foldr Or (head es) (tail es)

ex2 :: Expr String
ex2 = Or (Or (And (Var "a") (Var "b")) (Var "c")) (Or (Var "d") (Var "f"))


-- ex3 :: Expr (Int, Int, Int)
-- ex3 = And (Or (Or (Var (1,1,1)) Empty) Empty) (Or Empty (Var (2,1,1)))

-- data Foo = All [Foo]
--          | Any [Foo]
         
-- allAnd' :: 