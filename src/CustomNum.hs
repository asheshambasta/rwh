module CustomNum where
import Data.List

----------------------------------------------
-- Symbolic units/manipulation
----------------------------------------------

-- operators supported
data Op = Plus | Minus | Mul | Div | Pow
        deriving (Eq, Show)

data SymbolicManip a =
            Number a
          | Symbol String
          | BinaryArith Op (SymbolicManip a) (SymbolicManip a)
          | UnaryArith String (SymbolicManip a)
            deriving (Eq)

instance Num a => Num (SymbolicManip a) where
  a + b = BinaryArith Plus a b
  a - b = BinaryArith Minus a b
  a * b = BinaryArith Mul a  b
  negate = BinaryArith Mul (Number (-1))
  abs = UnaryArith "abs"
  signum _ = error "signup is not implemented"
  fromInteger = Number . fromInteger

