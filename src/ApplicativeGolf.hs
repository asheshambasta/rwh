module ApplicativeGolf where

sequenceB_ :: (Applicative f) => [f a] -> f [a]
sequenceB_ = foldr (\f1 -> (<*>) ((:) <$> f1)) (pure [])

