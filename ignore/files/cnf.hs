{-# OPTIONS -Wall #-}

module Main where

type Name = String

data Formula = Const Bool
             | Var Name
             | Not Formula
             | And Formula Formula
             | Or Formula Formula
             | Imply Formula Formula
             -- deriving Show 

elimImply :: Formula -> Formula
elimImply (Not p) = Not (elimImply p)
elimImply (And p q) = And (elimImply p) (elimImply q)
elimImply (Or p q) = Or (elimImply p) (elimImply q)
elimImply (Imply p q) = Or (Not (elimImply p)) (elimImply q)
elimImply p = p

deMorgan :: Formula -> Formula
deMorgan (Not (And p q)) = Or (deMorgan (Not p)) (deMorgan (Not q))
deMorgan (Not (Or p q)) = And (deMorgan (Not p)) (deMorgan (Not q))
deMorgan (Not (Not p)) = deMorgan p
deMorgan (Not p) = Not (deMorgan p)
deMorgan (And p q) = And (deMorgan p) (deMorgan q)
deMorgan (Or p q) = Or (deMorgan p) (deMorgan q)
deMorgan p = p

distOr :: Formula -> Formula
distOr (Or (And p q) r) = And (distOr (Or p r)) (distOr (Or q r))
distOr (Or p (And q r)) = And (distOr (Or p q)) (distOr (Or p r))
distOr (Or p q) = Or (distOr p) (distOr q) -- これだと、Or (Or (And . .) .) (Or . .)のとき最後まで分配できない
distOr (And p q) = And (distOr p) (distOr q)
distOr p = p

assocAndOr :: Formula -> Formula
assocAndOr (And p (And q r)) =
  assocAndOr (And (assocAndOr (And p q)) (assocAndOr r))
assocAndOr (And p q) = And (assocAndOr p) (assocAndOr q)
assocAndOr (Or p (Or q r)) =
  assocAndOr (Or (assocAndOr (Or p q)) (assocAndOr r))
assocAndOr (Or p q) = Or (assocAndOr p) (assocAndOr q)
assocAndOr p = p

cnf :: Formula -> Formula
cnf = assocAndOr . distOr . deMorgan . elimImply

string_True :: String
string_True = "True"
string_False :: String
string_False = "False"
string_Not :: String
string_Not = "~"
string_And :: String
string_And = "/\\"
string_Or :: String
string_Or = "\\/"
string_Imply :: String
string_Imply = "->"

prec_Not :: Int
prec_Not = 7
prec_And_r :: Int
prec_And_r = 6
prec_And :: Int
prec_And = 5
prec_Or_r :: Int
prec_Or_r = 4
prec_Or :: Int
prec_Or = 3
prec_Imply_l :: Int
prec_Imply_l = 2
prec_Imply :: Int
prec_Imply = 1

instance Show Formula where
  showsPrec _ (Const True) = showString string_True
  showsPrec _ (Const False) = showString string_False
  showsPrec _ (Var x) = showString x
  showsPrec d (Not p) = showParen (d > prec_Not) s
    where s = showString string_Not . showsPrec prec_Not p
  showsPrec d (And p q) = showParen (d > prec_And) s
    where s = showsPrec prec_And p .
              showString " " .
              showString string_And .
              showString " " .
              showsPrec prec_And_r q
  showsPrec d (Or p q) = showParen (d > prec_Or) s
    where s = showsPrec prec_Or p .
              showString " " .
              showString string_Or .
              showString " " .
              showsPrec prec_Or_r q
  showsPrec d (Imply p q) = showParen (d > prec_Imply) s
    where s = showsPrec prec_Imply_l p .
              showString " " .
              showString string_Imply .
              showString " " .
              showsPrec prec_Imply q

ex1 :: Formula
ex1 = Imply (Imply (And (Var "p") (Var "q")) (Var "r")) (Imply (Var "p") (Imply (Var "q") (Var "r")))

ex2 :: Formula
ex2 = Or (Or (And (Var "a") (Var "b")) (Var "c")) (Or (Var "d") (Var "f"))