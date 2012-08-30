{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Logic where

-- import Data.ByteString (ByteString)
-- import qualified Data.ByteString as BS
-- import Data.ByteString.Char8 () -- instance だけ読み込む
-- import Data.ByteString.Lazy (ByteString)
-- import qualified Data.ByteString.Lazy as BL


import Data.List
import Control.Monad.State

data Formula = Const Bool
             | Var VarNum
             | Not Formula
             | And Formula Formula
             | Or Formula Formula
             | Imply Formula Formula
             | Equiv Formula Formula
             deriving (Show, Eq)
                      
type VarNum = Int


type CNF = [Clause]
type Clause = [Literal]
data Literal = A VarNum 
             | N VarNum
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
elimConst (Imply p q) = case (elimConst p, elimConst q) of
  (Const True, r) -> r
  (Const False, _) -> Const True
  (_, Const True) -> Const True
  (r, Const False) -> Not r
  (r, s) -> Imply r s
elimConst (Equiv p q) = case (elimConst p, elimConst q) of
  (Const True, r) -> r
  (Const False, r) -> Not r
  (r, Const True) -> r
  (r, Const False) -> Not r
  (r, s) -> Equiv r s

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


isCNF :: Formula -> Bool
isCNF (Const _) = True
isCNF (Var _) = True
isCNF (Not p) = case p of
  Const _ -> True
  Var _ -> True
  _ -> False
isCNF (And p q) = isCNF p && isCNF q
isCNF (Or p q) = case (p, q) of
  (And _ _, _) -> False
  (_, And _ _) -> False
  _ -> isCNF p && isCNF q
isCNF _ = False

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
                     
-- 
toCNF :: Formula -> CNF
toCNF (Const b) = case b of
  True -> []
  False -> error "toCNF: Const False"
  -- error "cnf const" -- bimyou
toCNF (Var v) = [[A v]]
toCNF (Not p) = case p of
  Var v -> [[N v]]
  _ -> error "toCNF: not CNF"
toCNF (And p q) = case (p, q) of
  (Or _ _, r) -> [toClause p] ++ toCNF r
  (r, Or _ _) -> toCNF r ++ [toClause q]
  (_, _) -> toCNF p ++ toCNF q
toCNF (Or p q) = [toClause $ Or p q]  
toCNF _ = error "toCNF: not CNF"


allAnd :: [Formula] -> Formula
allAnd = foldr And (Const True)

allOr :: [Formula] -> Formula
allOr = foldr Or (Const False)

encode :: VarNum -> Formula -> String
encode n = encCNF . run n    

encode' :: Formula -> String
encode' = encCNF . toCNF . formulaCNF

encCNF :: CNF -> String
encCNF = concat . map toa

toa :: Clause -> String
toa = (++ " 0\n") . concat . intersperse " " . map show . map toNum

toNum :: Literal -> Int
toNum (A v) = v
toNum (N v) = negate v

tseitin :: Formula -> Formula -> State Int Formula
tseitin (Const b) var = do
  return $ formulaCNF $ Equiv var (Const b)
tseitin (Var v) var = do
  return $ formulaCNF $ Equiv var (Var v)
tseitin (Not p) var = case p of
  (Const b) -> return $ formulaCNF $ Equiv var (Const $ not b)
  (Var v) -> return $ formulaCNF $ Equiv var (Not $ Var v)
  _ -> do
    new <- newVar
    p' <- tseitin p new
    return $ And (formulaCNF $ Equiv var (Not new)) p'
tseitin (And p q) var = case (p, q) of
    (Const _, Const _) -> return $ formulaCNF $ Equiv var (And p q)
    (Const _, Var _) -> return $ formulaCNF $ Equiv var (And p q)
    (Const _, _) -> do
      new <- newVar
      q' <- tseitin q new
      return $ And (formulaCNF $ Equiv var (And p new)) q'
    (Var _, Const _) -> return $ formulaCNF $ Equiv var (And p q)
    (Var _, Var _) -> return $ formulaCNF $ Equiv var (And p q)
    (Var _, _) -> do
      new <- newVar
      q' <- tseitin q new
      return $ And (formulaCNF $ Equiv var (And p new)) q'
    (_, Const _) -> do
      new <- newVar
      p' <- tseitin p new
      return $ And (formulaCNF $ Equiv var (And new q)) p'
    (_, Var _) -> do
      new <- newVar
      p' <- tseitin p new
      return $ And (formulaCNF $ Equiv var (And new q)) p'
    (_, _) -> do
      newp <- newVar
      newq <- newVar
      p' <- tseitin p newp
      q' <- tseitin q newq
      return $ And (And (formulaCNF $ Equiv var (And newp newq)) p') q'
tseitin (Or p q) var = case (p, q) of
    (Const _, Const _) -> return $ formulaCNF $ Equiv var (Or p q)
    (Const _, Var _) -> return $ formulaCNF $ Equiv var (Or p q)
    (Const _, _) -> do
      new <- newVar
      q' <- tseitin q new
      return $ And (formulaCNF $ Equiv var (Or p new)) q'
    (Var _, Const _) -> return $ formulaCNF $ Equiv var (Or p q)
    (Var _, Var _) -> return $ formulaCNF $ Equiv var (Or p q)
    (Var _, _) -> do
      new <- newVar
      q' <- tseitin q new
      return $ And (formulaCNF $ Equiv var (Or p new)) q'
    (_, Const _) -> do
      new <- newVar
      p' <- tseitin p new
      return $ And (formulaCNF $ Equiv var (Or new q)) p'
    (_, Var _) -> do
      new <- newVar
      p' <- tseitin p new
      return $ And (formulaCNF $ Equiv var (Or new q)) p'
    (_, _) -> do
      newp <- newVar
      newq <- newVar
      p' <- tseitin p newp
      q' <- tseitin q newq
      return $ And (And (formulaCNF $ Equiv var (Or newp newq)) p') q'
tseitin (Imply p q) var = case (p, q) of
    (Const _, Const _) -> return $ formulaCNF $ Equiv var (Imply p q)
    (Const _, Var _) -> return $ formulaCNF $ Equiv var (Imply p q)
    (Const _, _) -> do
      new <- newVar
      q' <- tseitin q new
      return $ And (formulaCNF $ Equiv var (Imply p new)) q'
    (Var _, Const _) -> return $ formulaCNF $ Equiv var (Imply p q)
    (Var _, Var _) -> return $ formulaCNF $ Equiv var (Imply p q)
    (Var _, _) -> do
      new <- newVar
      q' <- tseitin q new
      return $ And (formulaCNF $ Equiv var (Imply p new)) q'
    (_, Const _) -> do
      new <- newVar
      p' <- tseitin p new
      return $ And (formulaCNF $ Equiv var (Imply new q)) p'
    (_, Var _) -> do
      new <- newVar
      p' <- tseitin p new
      return $ And (formulaCNF $ Equiv var (Imply new q)) p'
    (_, _) -> do
      newp <- newVar
      newq <- newVar
      p' <- tseitin p newp
      q' <- tseitin q newq
      return $ And (And (formulaCNF $ Equiv var (Imply newp newq)) p') q'
tseitin (Equiv p q) var = case (p, q) of
    (Const _, Const _) -> return $ formulaCNF $ Equiv var (Equiv p q)
    (Const _, Var _) -> return $ formulaCNF $ Equiv var (Equiv p q)
    (Const _, _) -> do
      new <- newVar
      q' <- tseitin q new
      return $ And (formulaCNF $ Equiv var (Equiv p new)) q'
    (Var _, Const _) -> return $ formulaCNF $ Equiv var (Equiv p q)
    (Var _, Var _) -> return $ formulaCNF $ Equiv var (Equiv p q)
    (Var _, _) -> do
      new <- newVar
      q' <- tseitin q new
      return $ And (formulaCNF $ Equiv var (Equiv p new)) q'
    (_, Const _) -> do
      new <- newVar
      p' <- tseitin p new
      return $ And (formulaCNF $ Equiv var (Equiv new q)) p'
    (_, Var _) -> do
      new <- newVar
      p' <- tseitin p new
      return $ And (formulaCNF $ Equiv var (Equiv new q)) p'
    (_, _) -> do
      newp <- newVar
      newq <- newVar
      p' <- tseitin p newp
      q' <- tseitin q newq
      return $ And (And (formulaCNF $ Equiv var (Equiv newp newq)) p') q'
  
newVar :: State Int Formula
newVar = state $ \x -> (Var $ x + 1, x + 1)
  
run :: VarNum -> Formula -> CNF                       
run maxVarNum formula = toCNF . And (Var next) . fst . runState (tseitin formula' (Var next)) $ next 
  where
    formula' = elimConst formula
    next = maxVarNum + 1 
      
           

formula :: Formula
formula = Or (Equiv (Var 4) (And (Imply (Var 3) (And (Var 6) (Var 7))) (Var 1))) (And (Var 2) (Var 5))

small :: Formula
small = allOr . map allAnd . divide 2 . map Var $ [1..4]


large :: Formula
large = allOr . map allAnd . divide 50 $ map Var [1..100]


divide :: Int -> [a] -> [[a]]
divide _ [] = []
divide n xs = left : divide n right
  where
    (left, right) = splitAt n xs
    
writeTseitin :: IO ()
writeTseitin = writeFile "tseitin" $ concat ["p cnf 500 3000", encode 100 large]

writeNormal :: IO ()
writeNormal = writeFile "normal" $ concat ["p cnf 100 3000", encode' large]