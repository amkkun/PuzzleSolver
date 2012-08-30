{-# OPTIONS -Wall #-}

module Logic where

type VarN = Int -- variable number
data Formula = Var VarN
             | Not Formula
             | And [Formula]
             | Or [Formula]  
             deriving (Show, Eq)

data Literal = A VarN | N VarN
type Clause = [Literal]
type CNF = [Clause]



toCNF :: Formula -> Formula
toCNF (Var n) = Var n
toCNF (Not p) = 
toCNF (And p q) = And (encode p) (encode q)

tseitin :: Int -> Formula -> Formula
tseitin gen (Var n) = toCNF $ Var n
tseitin gen (Not p) = let (var, gen') = newVar gen in toCNF $ Equiv [var, Not (tseitin gen' p)] 
tseitin gen (And ps) = let (var, gen') = newVar gen in toCNF $ Equiv [var, And (map tseitin ps)]
tseitin gen (Or ps) = let (var, gen') = newVar gen in toCNF $ Equiv [var, Or (map tseitin ps)]
tseitin gen (Imply ps) = let (var, gen') = newVar gen in toCNF $ Equiv [var, Imply (map (tseitin  ps)]

newVar :: VarN -> (Formula, VarN)
newVar gen = let next = succ gen in (Var next, next)

elimImply :: Formula -> Formula
elimImply (Var n) = Var n
elimImply (Not p) = Not $ elimImply p
elimImply (And ps) = And $ map elimImply ps
elimImply (Or ps) = Or $ map elimImply ps
elimImply (Imply []) = error "elimImply"
elimImply (Imply (p:ps)) = Or [Not $ elimImply p, elimImply $ Imply ps]

deMorgan :: Formula -> Formula
deMorgan (Not (Not p)) = deMorgan p
deMorgan (Not (And ps)) = Or $ map (deMorgan . Not) ps
deMorgan (Not (Or ps)) = And $ map (deMorgan . Not) ps
deMorgan (Not p) = Not $ deMorgan p
deMorgan (And ps) = And $ map deMorgan ps
deMorgan (Or ps) = Or $ map deMorgan ps
deMorgan p = p

distOr :: Formula -> Formula
distOr (And ps) = And $ map distOr ps
distOr (Or []) = error "distOr"
distOr (Or (p:ps)) = case p of
  (And qs) -> And $ distOr (Or p

-- Or [1, And [2, 3], Or [4,5]] -> 
--

sat :: CNF -> String
sat cnf = firstLine cnf ++ encodeCNF cnf

encodeCNF :: CNF -> String
encodeCNF = map encodeClause
  where
    encodeClause = (++ " 0\n") . intersperse ' ' . concat . map encodeLiteral
    encodeLiteral (A n) = show n
    encodeLiteral (N n) = show $ negate n

firstLine :: CNF -> String
firstLine cnf = "p cnf " ++ maxVarN cnf ++ " " ++ cnfLen cnf ++ "\n"
  where
    maxVarN = maximum . map toVarN . concat 
    toVarN (A n) = n
    toVarN (N n) = n
    cnfLen = show . length
    

