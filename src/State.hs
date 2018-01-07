{-# LANGUAGE TypeSynonymInstances #-}

module State where

import System.Random

newtype State s a = State {
  runState :: s -> (a, s)
}

returnState :: a -> State s a
returnState a = State $ \s -> (a, s)

bindState :: State s a -> (a -> State s b) -> State s b
bindState sa fa2sb =
  State func
  where func = \s ->
          let (a, s') = (runState sa) s
              sb = fa2sb a
          in (runState sb) s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ const ((), s)

type RandomState a = State StdGen a

--getRandom :: Random a => RandomState a
--getRandom =
--  get >>= \gen ->
--  let (val, gen') = random gen in
--  put gen' >>
--  return val

