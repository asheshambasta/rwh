module QC where

import Test.QuickCheck
import Prettify
import Control.Monad (liftM, liftM2)

--instance Arbitrary Char where
--  arbitrary = elements (['A'..'Z'] ++ ['a'..'z'] ++ " ~!@#$%^&*()")

instance Arbitrary Doc where
  arbitrary =
      oneof [
        return Empty,
        liftM Char arbitrary,
        liftM Text arbitrary,
        return Line,
        liftM2 Concat arbitrary arbitrary,
        liftM2 Union arbitrary arbitrary
      ]

prop_empty_id x =
    x <> empty == x
  &&
    empty <> x == x

prop_char c = char c == Char c

prop_text s = text s == Text s

prop_line = line == Line

prop_double d = double d == Text (show d)
