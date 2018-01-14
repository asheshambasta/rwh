import Control.Monad.Trans.Writer

logNumber x =  writer (x, ["got number " ++ show x])

multLog a b  = do
  la <- logNumber a
  lb <- logNumber b
  tell [show a ++ " * " ++ show b]
  return (la * lb)


gcd' a b
    | b == 0 = do
      tell ["finished with: " ++ show a]
      return a
    | otherwise = do
      tell ["trying with " ++ show a ++ ", " ++ show b]
      gcd' b (a `mod` b)
