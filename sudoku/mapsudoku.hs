import Data.Sequence (Seq)
import qualified Data.Sequence as S

boardsize :: Int
boardsize = 9
boxsize :: Int
boxsize = 3 
cellvals :: Choices
cellvals = S.fromList [1..9]
blank :: Val -> Bool
blank = (== 0) 


type Matrix a = Seq (Seq a)
type Sudoku = Matrix Val

type SeqMap k a = Seq (k, Seq a)

type Choices = Seq Val

type Val = Int
type Pos = (Int, Int)

type PosMap = SeqMap Pos Val
type ValMap = SeqMap Val Pos


-- * solve main loop
solve :: PosMap -> [PosMap]
solve pm
  | isFailed pm = []
  | isCompleted pm = return pm
  | otherwise = update pm >>= mainsolve . eliminate

isFailed :: PosMap -> Bool
isFailed pm = 
