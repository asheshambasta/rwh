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

simplify :: (Num a, Eq a) => SymbolicManip a -> SymbolicManip a
simplify (BinaryArith op ia ib) =
  let sa = simplify ia
      sb = simplify ib
  in case (op, sa, sb) of
      (Mul, Number 1, b) -> b
      (Mul, a, Number 1) -> a
      (Mul, Number 0, b) -> Number 0
      (Mul, b, Number 0) -> Number 0
      (Div, a, Number 1) -> a
      (Div, Number 1, b) -> b
      (Plus, a, Number 0) -> a
      (Plus, Number 0, b) -> b
      (Minus, a, Number 0) -> a
simplify (UnaryArith op a) = UnaryArith op (simplify a)
simplify x = x

data Units a = Units a (SymbolicManip a)
    deriving (Eq)

instance (Num a, Eq a) => Num (Units a) where
    (Units xa ua) + (Units xb ub)
        | ua == ub = Units (xa + xb) ua
        | otherwise = error "Mismatched units in add/subtract"
    (Units xa ua) - (Units xb ub) = Units xa ua + Units (xb * (-1)) ub
    (Units xa ua) * (Units xb ub) = Units (xa*xb) (ua*ub)
    negate (Units xa ua) = Units (negate xa) ua
    abs (Units xa ua) = Units (abs xa) ua
    signum (Units xa _) = Units (signum xa) (Number 1)
    fromInteger i = Units (fromInteger i) (Number 1)

instance (Fractional a, Eq a) => Fractional (Units a) where
  (Units xa ua) / (Units xb ub) = Units (xa/xb) (ua/ub)
  recip a = 1 / a
  fromRational r = Units (fromRational r) (Number 1)

instance (Floating a, Eq a) => Floating (Units a) where
    pi = Units pi (Number 1)
    exp _ = error "not implemented"
    log _ = error "not implemented"
    (Units xa ua) ** (Units xb ub) = Units (xa ** xb) (ua ** ub)
    sqrt (Units xa ua) = Units (sqrt xa) (sqrt ua)
    sin (Units xa ua)
        | ua == Symbol "rad" = Units (sin xa) (Number 1)
        | ua == Symbol "deg" = Units (sin (deg2rad xa)) (Number 1)
        | otherwise = error "units must be either deg or rad"
    cos (Units xa ua)
        | ua == Symbol "rad" = Units (cos xa) (Number 1)
        | ua == Symbol "deg" = Units (cos (deg2rad xa)) (Number 1)
        | otherwise = error "units must be either deg or rad"
    tan (Units xa ua)
        | ua == Symbol "rad" = Units (tan xa) (Number 1)
        | ua == Symbol "deg" = Units (tan (deg2rad xa)) (Number 1)
        | otherwise = error "units must be either deg or rad"
    asin (Units xa ua)
        | ua == Number 1 = Units (rad2deg $ asin xa) (Symbol "deg")
        | otherwise = error "units must be empty"
    acos (Units xa ua)
        | ua == Number 1 = Units (rad2deg $ acos xa) (Symbol "deg")
        | otherwise = error "units must be empty"
    atan (Units xa ua)
        | ua == Number 1 = Units (rad2deg $ atan xa) (Symbol "deg")
        | otherwise = error "units must be empty"
    sinh = error "not implemented"
    cosh = error "not implemented"
    tanh = error "not implemented"
    asinh = error "not implemented"
    acosh = error "not implemented"
    atanh = error "not implemented"

units :: (Num a) => a -> String -> Units a
units a u = Units a (Symbol u)

dropUnits :: (Num a) => Units a -> a
dropUnits (Units a _) = a

deg2rad x = 2 * pi * x / 360
rad2deg x = (360 * x) / (2 * pi)

test :: (Num a) => a
test = 2 * 5 + 3
