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
data Literal = A Var | N Var


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
distOr (Or p q) = case (p, q) of
  (And r s, _) -> And (distOr $ Or r q) (distOr $ Or s q)
  (_, And r s) -> And (distOr $ Or p r) (distOr $ Or p s)
  (_, _) -> Or (distOr p) (distOr q)
distOr _ = error "before distOr, do deMorgan"

formulaCNF :: Formula -> Formula
formulaCNF = distOr . elimConst . deMorgan . elimImply . elimEquiv

toLiterals :: Formula -> [Formula]
toLiterals (Const _) = error "invalid" -- case b of
  -- True = []
  -- False = error "toLiterals invalid"
toLiterals (Var v) = [Var v]
toLiterals (Not p) = [Not p]
toLiterals (And _ _) = error "invalid"
toLiterals (Or p q) = toLiterals p ++ toLiterals q
toLiterals _ = error "invalid"
                     
toClauses :: Formula -> [[Formula]]
toClauses (Const _) = [] -- bimyou
toClauses (Var v) = [[Var v]]
toClauses (Not p) = [[Not p]]
toClauses (And p q) = case (p, q) of
  (Or _ _, r) -> [toLiterals p] ++ toClauses r
  (r, Or _ _) -> toClauses r ++ [toLiterals p]
  (_, _) -> toClauses p ++ toClauses q
toClauses (Or p q) = [toLiterals $ Or p q]  
toClauses _ = error "invalid"
    
-- toCNF :: Formula -> CNF
-- toCNF (Const b) = []
-- toCNF (Var v) = A v
-- toCNF (Not p) = case p of
--   Var v = N v
--   _ = error "not in var igaiha hairanaiyo"
-- toCNF (And p q) = case (p, q) of
--   (Or r s, Or t u) = map (map toCNF) [[r, s], [t, u]]
--   (Or r s, And t u) = [t, u] ++ map toCNF
    
    
    