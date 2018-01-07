module MonoidGolf where

import qualified Data.Foldable as F

data MValue = SomeVal Int | Unity

mult :: MValue -> MValue -> MValue
mult (SomeVal x) (SomeVal y) = SomeVal (x*y)
mult (SomeVal x) Unity = SomeVal x
mult Unity (SomeVal y) = SomeVal y

instance Monoid MValue where
  mempty = Unity
  mappend = mult


lengthCompare :: String -> String -> Ordering
lengthCompare s1 s2 = (l1 `compare` l2) `mappend` (s1 `compare` s2)
  where l1 = length s1
        l2 = length s2

-- just an example of a Maybe Monoid
{-instance Monoid a => Monoid (Maybe a) where
  mempty = Nothing
  Nothing `mappend` m = m
  m `mappend` Nothing = m
  (Just a1) `mappend` (Just a2) = Just (a1 `mappend` a2)-}

newtype First a = First { getFirst :: Maybe a }
    deriving (Eq, Ord, Read, Show)

instance Monoid (First a) where
  mempty = First Nothing
  (First Nothing) `mappend` m = m
  m `mappend` _ = m

data Tree2 a = Empty | Node a (Tree2 a) (Tree2 a)

instance Functor Tree2 where
  fmap f Empty = Empty
  fmap f (Node a t1 t2) = Node (f a) (fmap f t1) (fmap f t2)

instance F.Foldable Tree2 where
--  foldMap :: (Monoid m, F.Foldable t) => (a -> m) -> t a -> m
  foldMap f Empty = mempty
  foldMap f (Node a t1 t2) =  foldMap f t1  `mappend`
                              f a           `mappend`
                              foldMap f t2

