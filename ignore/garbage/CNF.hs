{-# OPTIONS -Wall #-}

data CNF = Empty 
         | And CNF CNF
         | Or Clause Clause
         | Not Variable

data Clause = 