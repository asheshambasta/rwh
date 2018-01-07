module MonadGolf where

newtype Maybe2 a = Maybe2 { get :: Maybe a }
                deriving (Show, Eq)

instance Functor Maybe2 where
  fmap f (Maybe2 (Just x)) = Maybe2 (Just (f x))
  fmap _ (Maybe2 Nothing) = Maybe2 Nothing

instance Applicative Maybe2 where
  pure x = Maybe2 (Just x)
  Maybe2 (Just fab) <*> Maybe2 (Just a) = Maybe2 (Just (fab a))
  _ <*> _ = Maybe2 Nothing

instance Monad Maybe2 where
  return = pure
  Maybe2 (Just x) >>= f = f x
  _ >>= _ = Maybe2 Nothing
  fail _ = Maybe2 Nothing
