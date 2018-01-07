module MonoidGolf where

data MValue = SomeVal Int | Unity

mult :: MValue -> MValue -> MValue
mult (SomeVal x) (SomeVal y) = SomeVal (x*y)
mult (SomeVal x) Unity = SomeVal x
mult Unity (SomeVal y) = SomeVal y

instance Monoid MValue where
  mempty = Unity
  mappend = mult
