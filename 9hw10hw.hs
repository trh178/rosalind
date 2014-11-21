import Data.List
import Data.Char
import Unsafe.Coerce

data Nat = Zero | Succ Nat deriving Show

natToInteger :: Nat -> Integer
natToInteger = \ n -> length [c | c <- show n, c == 'S']