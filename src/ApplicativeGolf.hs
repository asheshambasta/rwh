module ApplicativeGolf where

sequenceB_ :: (Applicative f) => [f a] -> f [a]
sequenceB_ = foldr (\f1 -> (<*>) ((:) <$> f1)) (pure [])

sequenceB2_ :: (Applicative f) => [f a] -> f [a]
sequenceB2_ [] = pure []
sequenceB2_ (f1:rest) = (:) <$> f1 <*> sequenceB2_ rest

sequenceB3_ :: (Applicative f) => [f a] -> f [a]
sequenceB3_ = foldr chain (pure [])
    where chain f1 = (<*>) ((:) <$> f1)
