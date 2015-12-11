
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Unique (Unique, unUnique, newUnique, evalUnique)where
import Control.Monad.State

newtype Unique a = Unique {unUnique::(State [Int] a)}
    deriving(Functor, Applicative, Monad, MonadState [Int])

modifyAndGet :: MonadState s m => (s -> (a, s)) -> m a
modifyAndGet f = state (\s -> f s)

newUnique :: Unique Int
newUnique = modifyAndGet (\(x:xs)->(x, xs))

evalUnique :: Unique a -> a
evalUnique = flip evalState [0..] . unUnique
