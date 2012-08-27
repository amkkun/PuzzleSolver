{-# OPTIONS -Wall #-}

import Control.Applicative (
  (<$>)
  )

data Formula = Const Bool
             | Var Var
             | Not Formula
             | And Formula Formula
             | Or Formula Formula
             | Imply Formula Formula
             | Equiv Formula Formula
             deriving (Show, Eq)
                      
type Var = Int


type CNF = [Clause]
type Clause = [Literal]
data Literal = A Var 
             | N Var
             deriving (Show, Eq)          

elimEquiv :: Formula -> Formula
elimEquiv (Const b) = Const b
elimEquiv (Var v) = Var v
elimEquiv (Not p) = Not (elimEquiv p)
elimEquiv (And p q) = And (elimEquiv p) (elimEquiv q) 
elimEquiv (Or p q) = Or (elimEquiv p) (elimEquiv q)
elimEquiv (Imply p q) = Imply (elimEquiv p) (elimEquiv q)
elimEquiv (Equiv p q) = And (Imply (elimEquiv p) (elimEquiv q)) (Imply (elimEquiv q) (elimEquiv p))

elimImply :: Formula -> Formula
elimImply (Const b) = Const b
elimImply (Var v) = Var v
elimImply (Not p) = Not (elimImply p)
elimImply (And p q) = And (elimImply p) (elimImply q)
elimImply (Or p q) = Or (elimImply p) (elimImply q)
elimImply (Imply p q) = Or (Not (elimImply p)) (elimImply q)
elimImply (Equiv _ _) = error "先にelimEquivやってくれ"

elimConst :: Formula -> Formula
elimConst (Const b) = Const b
elimConst (Var v) = Var v
elimConst (Not p) = Not (elimConst p)
elimConst (And p q) = case (elimConst p, elimConst q) of
  (Const True, r) -> r
  (Const False, _) -> Const False
  (r, Const True) -> r  
  (_, Const False) -> Const False
  (r, s) -> And r s
elimConst (Or p q) = case (elimConst p, elimConst q) of
  (Const True, _) -> Const True
  (Const False, r) -> r
  (_, Const True) -> Const True  
  (r, Const False) -> r
  (r, s) -> Or r s
elimConst _ = error "elimConst"

deMorgan :: Formula -> Formula
deMorgan (Const b) = Const b
deMorgan (Var v) = Var v
deMorgan (Not p) = case p of
  Const b -> Const $ not b
  Var v -> Not $ Var v
  Not q -> deMorgan q
  And q r -> Or (deMorgan $ Not q) (deMorgan $ Not r)
  Or q r -> And (deMorgan $ Not q) (deMorgan $ Not r) 
  _ -> error "before deMorgan, do elimEquiv and elimImply"
deMorgan (And p q) = And (deMorgan p) (deMorgan q)
deMorgan (Or p q) = Or (deMorgan p) (deMorgan q)
deMorgan _ = error "before deMorgan, do elimEquiv and elimImply"

distOr :: Formula -> Formula
distOr (Const b) = Const b
distOr (Var v) = Var v
distOr (Not p) = Not $ distOr p
distOr (And p q) = And (distOr p) (distOr q)
distOr (Or p q) = case distOr p of
  And r s -> And (distOr $ Or r q) (distOr $ Or s q)
  r -> case distOr q of 
    And s t -> And (distOr $ Or r s) (distOr $ Or r t)
    s -> Or r s
distOr _ = error "before distOr, do deMorgan"

formulaCNF :: Formula -> Formula
formulaCNF = distOr . elimConst . deMorgan . elimImply . elimEquiv

toClause :: Formula -> Clause
toClause (Const _) = error "clause const" -- case b of
  -- True = []
  -- False = error "toClause invalid"
toClause (Var v) = [A v]
toClause (Not p) = case p of
  Var v -> [N v]
  _ -> error "clause not"
toClause (And _ _) = error "clause and"
toClause (Or p q) = toClause p ++ toClause q
toClause _ = error "clause other"
                     
toCNF :: Formula -> CNF
toCNF (Const _) = [] -- bimyou
toCNF (Var v) = [[A v]]
toCNF (Not p) = case p of
  Var v -> [[N v]]
  _ -> error "iimva"
toCNF (And p q) = case (p, q) of
  (Or _ _, r) -> [toClause p] ++ toCNF r
  (r, Or _ _) -> toCNF r ++ [toClause p]
  (_, _) -> toCNF p ++ toCNF q
toCNF (Or p q) = [toClause $ Or p q]  
toCNF _ = error "invalid"


cnf :: Formula -> CNF
cnf = toCNF . formulaCNF
-- toCNF :: Formula -> CNF
-- toCNF (Const b) = []
-- toCNF (Var v) = A v
-- toCNF (Not p) = case p of
--   Var v = N v
--   _ = error "not in var igaiha hairanaiyo"
-- toCNF (And p q) = case (p, q) of
--   (Or r s, Or t u) = map (map toCNF) [[r, s], [t, u]]
--   (Or r s, And t u) = [t, u] ++ map toCNF
    
    
    