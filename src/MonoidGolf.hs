module MonoidGolf where

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
