{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module MyParser where

newtype State s a = State {runState :: s -> (a, s)}

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f (State x) = State $ \state -> let (result, state') = x state in (f result, state')

instance Applicative (State s) where
  pure :: a -> State s a
  pure x = State (x,)

  (<*>) :: State s (a -> b) -> State s a -> State s b
  (State f) <*> (State x) = State $ \state0 ->
    let (result1, state1) = f state0
        (result2, state2) = x state1
     in (result1 result2, state2)

instance Monad (State s) where
  (>>=) :: State s a -> (a -> State s b) -> State s b
  (State x) >>= f = State $ \state0 ->
    let (result1, state1) = x state0
     in runState (f result1) state1

type Parser a = State (String, Int) a