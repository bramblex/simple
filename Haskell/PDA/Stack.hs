
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Stack where

newtype Stack a = Stack {unStack::[a]}
    deriving(Eq, Show, Read)

class StackClass t a where
    fromList :: [a] -> t a
    toList :: t a -> [a]

    top :: t a -> a
    top = head . toList
    pop :: t a -> t a
    pop = fromList . tail . toList
    push :: a -> t a -> t a
    push n = fromList . (n:) . toList

instance StackClass Stack a where
    fromList l = Stack l
    toList = unStack

instance Functor Stack where
    fmap f a = fromList . fmap f . toList $ a
