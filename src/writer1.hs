import Control.Monad.Trans.Writer

logNumber x =  writer (x, ["got number " ++ show x])

multLog a b  = do
  la <- logNumber a
  lb <- logNumber b
  return (la * lb)
