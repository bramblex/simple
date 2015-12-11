
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Stack where
import Control.Monad.State

newtype Stack s a = Stack {unStack::(State [s] a)}
    deriving(Functor, Applicative, Monad, MonadState [s])

evalStack :: [s] -> Stack s a -> a
evalStack stack = flip evalState stack . unStack

execStack :: [s] -> Stack s a -> [s]
execStack stack = flip execState stack . unStack

runStack :: [s] -> Stack s a -> (a, [s])
runStack stack = flip runState stack . unStack

top :: Stack a a
top = gets head

pop :: Stack s ()
pop = modify tail

push :: s -> Stack s ()
push n = modify (n:)
