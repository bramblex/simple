
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader

data Op = Plus | Minus | Mul | Div
    deriving (Eq)

instance Show Op where
    show op = case op of
                 Plus -> "+"
                 Minus -> "-"
                 Mul -> "×"
                 Div -> "÷"

data Expr = Number {value::Float}
          | Expr Op Expr Expr
          deriving (Eq)

instance Show Expr where
    show (Number n) =
        if n == fromInteger (round n) 
        then show (round n)
        else show n
    show (Expr op left right) = "(" ++ show left ++ show op ++ show right ++ ")"

instance Num Expr where
    (+) = Expr Plus
    (-) = Expr Minus
    (*) = Expr Mul

    abs = undefined
    signum = undefined
    fromInteger = Number . fromInteger

instance Fractional Expr where
    (/) = Expr Div
    fromRational = Number . fromRational

class Reducible a where
    reducible :: a -> Bool
    reduce :: a -> a

instance Reducible Expr where
    reducible (Number _) = False 
    reducible _ = True

    reduce (Expr op left right)
        | reducible left = Expr op (reduce left) right
        | reducible right = Expr op left (reduce right)
        | otherwise =  Number ((value left) `sym` (value right))
            where sym = case op of 
                            Plus -> (+)
                            Minus -> (-)
                            Mul -> (*)
                            Div -> (/)
            
type OutPut = [Expr]
type Machine a =  ReaderT Expr (Writer OutPut) a

newtype Comp a = Comp { unComp:: Machine a}
    deriving (Functor, Applicative, Monad, MonadReader Expr, MonadWriter OutPut)

eval :: Comp ()
eval = do
    expr <- ask
    tell [expr]
    case reducible expr of
        False -> return ()
        True -> local (const (reduce expr)) eval

execMachine :: Expr -> OutPut
execMachine = execWriter . runReaderT (unComp eval)

evalExpr :: Expr -> IO ()
evalExpr = mapM_ print . execMachine 
