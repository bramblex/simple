
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Unique (Unique, unUnique, newUnique, evalUnique)where
import Control.Monad.State

newtype Unique a = Unique {unUnique::(State Int a)}
    deriving(Functor, Applicative, Monad, MonadState Int)

newUnique :: Unique Int
newUnique = do
    state <- get
    modify (succ)
    return state

evalUnique :: Unique a -> a
evalUnique = flip evalState 0 . unUnique
