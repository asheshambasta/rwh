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

instance (Num a) => Num (SymbolicManip a) where
  a + b = BinaryArith Plus a b
  a - b = BinaryArith Minus a b
  a * b = BinaryArith Mul a  b
  negate = BinaryArith Mul (Number (-1))
  abs = UnaryArith "abs"
  signum _ = error "signum is not implemented"
  fromInteger = Number . fromInteger

instance (Fractional a) => Fractional (SymbolicManip a) where
  a / b = BinaryArith Div a b
  recip = BinaryArith Div (Number 1)
  fromRational r = Number (fromRational r)

instance (Floating a) => Floating (SymbolicManip a) where
  pi = Symbol "pi"
  exp = UnaryArith "exp"
  log = UnaryArith "log"
  sqrt = UnaryArith "sqrt"
  a ** b = BinaryArith Pow a b
  sin = UnaryArith "sin"
  cos = UnaryArith "cos"
  tan = UnaryArith "tan"
  asin = UnaryArith "asin"
  acos = UnaryArith "acos"
  atan = UnaryArith "atan"
  sinh  = UnaryArith "sinh"
  cosh = UnaryArith "cosh"
  tanh = UnaryArith "tanh"
  asinh  = UnaryArith "asinh"
  acosh = UnaryArith "acosh"
  atanh = UnaryArith "atanh"

prettyShow :: (Show a, Num a) => SymbolicManip a -> String
prettyShow (Number x) = show x
prettyShow (Symbol s) = s

prettyShow (BinaryArith op a b) =
      let pa = simpleParen a
          pb = simpleParen b
          pop = op2Str op
      in pa ++ pop ++ pb

op2Str Plus = "+"
op2Str Minus = "-"
op2Str Mul = "*"
op2Str Div = "/"
op2Str Pow = "**"

simpleParen :: (Show a, Num a) => SymbolicManip a -> String
simpleParen (Number x) = prettyShow (Number x)
simpleParen (Symbol s) = prettyShow (Symbol s)
simpleParen x@BinaryArith {} = "(" ++ prettyShow x ++ ")"
simpleParen x@UnaryArith {} = prettyShow x

instance (Show a, Num a) => Show (SymbolicManip a) where
  show = prettyShow

rpnShow :: (Show a, Num a) => SymbolicManip a -> String
rpnShow i =
  let toList (Number x) = [show x]
      toList (Symbol x) = [x]
      toList (BinaryArith op a b) = toList a ++ toList b ++ [op2Str op]
      toList (UnaryArith opStr a) = toList a ++ [opStr]
  in unwords (toList i)
