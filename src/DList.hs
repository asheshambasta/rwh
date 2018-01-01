module DList where

newtype DList a = DL {
  unDL :: [a] -> [a]
}

append :: DList a -> DList a -> DList a
append (DL u1) (DL u2) = DL (u1 . u2)

fromList :: [a] -> DList a
fromList as = DL (as ++)

toList :: DList a -> [a]
toList (DL u) = u []

empty :: DList a
empty = DL id

cons :: a -> DList a -> DList a
cons a (DL u) = DL ((a : ) . u)
infixr `cons`

dfoldr :: (a -> b -> b) -> b -> DList a -> b
dfoldr f s l = foldr f s (toList l)

dmap :: (a -> b) -> DList a -> DList b
dmap f as = DL (map f (toList as) ++)

safeHead :: DList a -> Maybe a
safeHead dl =
  case toList dl of
    (a:_) -> Just a
    _ -> Nothing

instance Functor DList where
  fmap = dmap

instance Monoid (DList a) where
  mempty = empty -- id element
  mappend = append -- is associative
