module SimpleState where

type SimpleState s a = s -> (a, s)
type StringState a = SimpleState String a

returnSt :: a -> SimpleState s a
returnSt a s = (a, s) -- was returnSt a = \s -> (a, s)

bindSt :: SimpleState s a -> (a -> SimpleState s b) -> SimpleState s b
bindSt sa f = \s -> let (a, s') = (sa s)
                        sb = f a
                    in sb s'

getSt :: SimpleState s s
getSt s = (s, s)

putSt :: SimpleState s ()
putSt s = ((), s)
