{-# OPTIONS -Wall #-}
module Formula
       ( Formula (..)
       , VarNum
       , allAnd
       , allOr
       , elimUseless
       , isCNFFormula
       , toCNF
       , dimacsTseitin
       , dimacsNormal
       , toDimacsString
       ) where

import Data.List
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as C
import Control.Monad.State

type VarNum = Int

data Formula = Const Bool
             | Var VarNum
             | Not Formula
             | And Formula Formula
             | Or Formula Formula
             | Imply Formula Formula
             | Equiv Formula Formula
             deriving (Show, Eq)

type CNF = [Clause]
type Clause = [Literal]
data Literal = A VarNum 
             | N VarNum
             deriving (Show, Eq)          

  
elimUseless :: Formula -> Formula
elimUseless (Const b) = Const b
elimUseless (Var v) = Var v
elimUseless (Not p) = Not (elimUseless p)
elimUseless (And p q) = case (elimUseless p, elimUseless q) of
  (Const True, r) -> r
  (Const False, _) -> Const False
  (r, Const True) -> r  
  (_, Const False) -> Const False
  (r, s) -> And r s
elimUseless (Or p q) = case (elimUseless p, elimUseless q) of
  (Const True, _) -> Const True
  (Const False, r) -> r
  (_, Const True) -> Const True  
  (r, Const False) -> r
  (r, s) -> Or r s
elimUseless (Imply p q) = case (elimUseless p, elimUseless q) of
  (Const True, r) -> r
  (Const False, _) -> Const True
  (_, Const True) -> Const True
  (r, Const False) -> Not r
  (r, s) -> Or (Not r) s 
elimUseless (Equiv p q) = case (elimUseless p, elimUseless q) of
  (Const True, r) -> r
  (Const False, r) -> Not r
  (r, Const True) -> r
  (r, Const False) -> Not r
  (r, s) -> And (Or (Not r) s) (Or (Not s) r)

deMorgan :: Formula -> Formula
deMorgan (Const b) = Const b
deMorgan (Var v) = Var v
deMorgan (Not p) = case p of
  Const b -> Const $ not b
  Var v -> Not $ Var v
  Not q -> deMorgan q
  And q r -> Or (deMorgan $ Not q) (deMorgan $ Not r)
  Or q r -> And (deMorgan $ Not q) (deMorgan $ Not r) 
  _ -> error "distOr: before deMorgan, do elimUseless"
deMorgan (And p q) = And (deMorgan p) (deMorgan q)
deMorgan (Or p q) = Or (deMorgan p) (deMorgan q)
deMorgan _ = error "distOr: before deMorgan, do elimUseless"

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
distOr _ = error "distOr: before distOr, do deMorgan"

toCNFFormula :: Formula -> Formula
toCNFFormula = distOr . deMorgan . elimUseless

isCNFFormula :: Formula -> Bool
isCNFFormula (Const _) = True
isCNFFormula (Var _) = True
isCNFFormula (Not p) = case p of
  Const _ -> True
  Var _ -> True
  _ -> False
isCNFFormula (And p q) = isCNFFormula p && isCNFFormula q
isCNFFormula (Or p q) = case (p, q) of
  (And _ _, _) -> False
  (_, And _ _) -> False
  _ -> isCNFFormula p && isCNFFormula q
isCNFFormula _ = False

-- # Formula -> CNF
-- suppose not contain `And' and `Const'
toClause :: Formula -> Clause
toClause (Const _) = error "toClause: contain Const"
toClause (Var v) = [A v]
toClause (Not p) = case p of
  Var v -> [N v]
  _ -> error "toClause: It is not CNF"
toClause (And _ _) = error "toClasuse: It is not CNF"
toClause (Or p q) = toClause p ++ toClause q
toClause _ = error "toClause: It is not CNF"
                     
-- suppose CNF Formula
toCNF :: Formula -> CNF
toCNF (Const b) = case b of
  True -> []
  False -> error "toCNF: Const False"
toCNF (Var v) = [[A v]]
toCNF (Not p) = case p of
  Var v -> [[N v]]
  _ -> error "toCNF: It is not CNF"
toCNF (And p q) = case (p, q) of
  (Or _ _, r) -> [toClause p] ++ toCNF r
  (r, Or _ _) -> toCNF r ++ [toClause q]
  (_, _) -> toCNF p ++ toCNF q
toCNF (Or p q) = [toClause $ Or p q]  
toCNF _ = error "toCNF: It is not CNF"

-- # make Formula
allAnd :: [Formula] -> Formula
allAnd = foldr And (Const True)

allOr :: [Formula] -> Formula
allOr = foldr Or (Const False)

-- # Tseitin
tseitin :: Formula -> Formula -> State Int Formula
tseitin (Const b) var = do
  return $ toCNFFormula $ Equiv var (Const b)
tseitin (Var v) var = do
  return $ toCNFFormula $ Equiv var (Var v)
tseitin (Not p) var = case p of
  (Const b) -> return $ toCNFFormula $ Equiv var (Const $ not b)
  (Var v) -> return $ toCNFFormula $ Equiv var (Not $ Var v)
  _ -> do
    new <- newVar
    p' <- tseitin p new
    return $ And (toCNFFormula $ Equiv var (Not new)) p'
tseitin (And p q) var = case (p, q) of
    (Const _, Const _) -> return $ toCNFFormula $ Equiv var (And p q)
    (Const _, Var _) -> return $ toCNFFormula $ Equiv var (And p q)
    (Const _, _) -> do
      new <- newVar
      q' <- tseitin q new
      return $ And (toCNFFormula $ Equiv var (And p new)) q'
    (Var _, Const _) -> return $ toCNFFormula $ Equiv var (And p q)
    (Var _, Var _) -> return $ toCNFFormula $ Equiv var (And p q)
    (Var _, _) -> do
      new <- newVar
      q' <- tseitin q new
      return $ And (toCNFFormula $ Equiv var (And p new)) q'
    (_, Const _) -> do
      new <- newVar
      p' <- tseitin p new
      return $ And (toCNFFormula $ Equiv var (And new q)) p'
    (_, Var _) -> do
      new <- newVar
      p' <- tseitin p new
      return $ And (toCNFFormula $ Equiv var (And new q)) p'
    (_, _) -> do
      newp <- newVar
      newq <- newVar
      p' <- tseitin p newp
      q' <- tseitin q newq
      return $ And (And (toCNFFormula $ Equiv var (And newp newq)) p') q'
tseitin (Or p q) var = case (p, q) of
    (Const _, Const _) -> return $ toCNFFormula $ Equiv var (Or p q)
    (Const _, Var _) -> return $ toCNFFormula $ Equiv var (Or p q)
    (Const _, _) -> do
      new <- newVar
      q' <- tseitin q new
      return $ And (toCNFFormula $ Equiv var (Or p new)) q'
    (Var _, Const _) -> return $ toCNFFormula $ Equiv var (Or p q)
    (Var _, Var _) -> return $ toCNFFormula $ Equiv var (Or p q)
    (Var _, _) -> do
      new <- newVar
      q' <- tseitin q new
      return $ And (toCNFFormula $ Equiv var (Or p new)) q'
    (_, Const _) -> do
      new <- newVar
      p' <- tseitin p new
      return $ And (toCNFFormula $ Equiv var (Or new q)) p'
    (_, Var _) -> do
      new <- newVar
      p' <- tseitin p new
      return $ And (toCNFFormula $ Equiv var (Or new q)) p'
    (_, _) -> do
      newp <- newVar
      newq <- newVar
      p' <- tseitin p newp
      q' <- tseitin q newq
      return $ And (And (toCNFFormula $ Equiv var (Or newp newq)) p') q'
tseitin (Imply p q) var = case (p, q) of
    (Const _, Const _) -> return $ toCNFFormula $ Equiv var (Imply p q)
    (Const _, Var _) -> return $ toCNFFormula $ Equiv var (Imply p q)
    (Const _, _) -> do
      new <- newVar
      q' <- tseitin q new
      return $ And (toCNFFormula $ Equiv var (Imply p new)) q'
    (Var _, Const _) -> return $ toCNFFormula $ Equiv var (Imply p q)
    (Var _, Var _) -> return $ toCNFFormula $ Equiv var (Imply p q)
    (Var _, _) -> do
      new <- newVar
      q' <- tseitin q new
      return $ And (toCNFFormula $ Equiv var (Imply p new)) q'
    (_, Const _) -> do
      new <- newVar
      p' <- tseitin p new
      return $ And (toCNFFormula $ Equiv var (Imply new q)) p'
    (_, Var _) -> do
      new <- newVar
      p' <- tseitin p new
      return $ And (toCNFFormula $ Equiv var (Imply new q)) p'
    (_, _) -> do
      newp <- newVar
      newq <- newVar
      p' <- tseitin p newp
      q' <- tseitin q newq
      return $ And (And (toCNFFormula $ Equiv var (Imply newp newq)) p') q'
tseitin (Equiv p q) var = case (p, q) of
    (Const _, Const _) -> return $ toCNFFormula $ Equiv var (Equiv p q)
    (Const _, Var _) -> return $ toCNFFormula $ Equiv var (Equiv p q)
    (Const _, _) -> do
      new <- newVar
      q' <- tseitin q new
      return $ And (toCNFFormula $ Equiv var (Equiv p new)) q'
    (Var _, Const _) -> return $ toCNFFormula $ Equiv var (Equiv p q)
    (Var _, Var _) -> return $ toCNFFormula $ Equiv var (Equiv p q)
    (Var _, _) -> do
      new <- newVar
      q' <- tseitin q new
      return $ And (toCNFFormula $ Equiv var (Equiv p new)) q'
    (_, Const _) -> do
      new <- newVar
      p' <- tseitin p new
      return $ And (toCNFFormula $ Equiv var (Equiv new q)) p'
    (_, Var _) -> do
      new <- newVar
      p' <- tseitin p new
      return $ And (toCNFFormula $ Equiv var (Equiv new q)) p'
    (_, _) -> do
      newp <- newVar
      newq <- newVar
      p' <- tseitin p newp
      q' <- tseitin q newq
      return $ And (And (toCNFFormula $ Equiv var (Equiv newp newq)) p') q'
  
newVar :: State Int Formula
newVar = state $ \x -> (Var $ x + 1, x + 1)
  
runTseitin :: Formula -> CNF                       
runTseitin formula = 
  toCNF . And (Var next) . fst . runState (tseitin formula' (Var next)) $ next
  where
    formula' = elimUseless formula
    next = maxVarNumF formula + 1

-- # maximum variable number
maxVarNumF :: Formula -> VarNum
maxVarNumF formula 
  | vnl == [] = 0
  | otherwise = maximum vnl
  where
    vnl = varNumListF formula
    
varNumListF :: Formula -> [VarNum]
varNumListF (Const _) = []
varNumListF (Var v) = [v]
varNumListF (Not p) = varNumListF p
varNumListF (Or p q) = varNumListF p ++ varNumListF q
varNumListF (And p q) = varNumListF p ++ varNumListF q
varNumListF (Imply p q) = varNumListF p ++ varNumListF q
varNumListF (Equiv p q) = varNumListF p ++ varNumListF q

maxVarNumC :: CNF -> VarNum
maxVarNumC cnf 
  | vnl == [] = 0
  | otherwise = maximum vnl
  where
    vnl = varNumListC cnf
    
varNumListC :: CNF -> [VarNum]
varNumListC [] = []
varNumListC ([]:cnf) = varNumListC cnf
varNumListC ((lit:cl):cnf) = abs (toNum lit) : varNumListC (cl:cnf) 

toNum :: Literal -> Int
toNum (A v) = v
toNum (N v) = negate v

-- # Encode DIMACS format
dimacsTseitin :: Formula -> ByteString
dimacsTseitin = toDimacsString . runTseitin 

dimacsNormal :: Formula -> ByteString
dimacsNormal = toDimacsString . toCNF . toCNFFormula

toDimacsString :: CNF -> ByteString
toDimacsString cnf = C.concat $ firstLine : map clauseString cnf
  where
    clauseNum = length cnf
    varNum = maxVarNumC cnf
    firstLine = C.pack $ "p cnf " ++ show varNum ++ " " ++ show clauseNum ++ "\n"
      
clauseString :: Clause -> ByteString
clauseString = C.pack . (++ " 0\n") . concat . intersperse " " . map show . map toNum

