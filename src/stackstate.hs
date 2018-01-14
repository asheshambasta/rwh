import Control.Monad.State

type Stack = [Int]


pop :: State Stack Int
pop = state $ \(x:xs) -> (x, xs)

push :: Int -> State Stack ()
push x = state $ \xs -> ((), x:xs)
