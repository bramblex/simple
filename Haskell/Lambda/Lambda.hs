
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lambda where
import Environment
import Template

data Expr = Var String | Lam String Expr | App Expr Expr
          deriving (Eq)

data Binding = Binding String Expr
    deriving (Eq)

type Program = [Binding]

class Reducible a where
    reducible :: a -> Bool
    reduce :: a -> Env a -> (a, Env a)

class Callable a where
    callable :: a -> Bool
    replace :: a -> String -> a -> a 

instance Callable Expr where
    callable (Lam _ _) = True 
    callable _ = False

    replace self@(Var v) name expr = 
        if v == name then expr else self
    replace self@(Lam v e) name expr =
        if v == name then self else Lam v (replace e name expr)
    replace self@(App e1 e2) name expr =
        App (replace e1 name expr) (replace e2 name expr)

instance Reducible Expr where
    reducible (App e1 e2) = (reducible e1) || (callable e1)
    reducible (Var _) = True
    reducible _ = False

    reduce (App e1 e2) env 
        | reducible e1 = (App (fst $ reduce e1 env) e2, env)
        {-| reducible e2 = (App e1 (fst $ reduce e2 env), env)-}
        | otherwise = (replace' e1 e2, env)
            where replace' (Lam name expr) e2 = replace expr name e2
    reduce (Var name) env = (lookupInEnv name env, env)

class Inspect a where
    inspect :: a -> String

instance Inspect Expr where
    inspect (Var v) = v
    inspect (Lam v expr) = render "λ#{v}.#{e}" [("v", v), ("e", show expr)]
    inspect (App e1 e2) = render "#{e1} #{e2}" [("e1", show e1), ("e2", show e2)]

instance Show Expr where
    show (App e1 e2) = render "#{e1} #{e2}" [("e1", showl e1), ("e2", showr e2)]
        where showr a@(Var _) = show a
              showr a = "("++ show a ++")"
              showl a@(Lam _ _) = "("++ show a ++")"
              showl a = show a
    show a = inspect a

instance Show Binding where
    show (Binding name expr) =
        render "#{name}=#{expr}" [("name", name), ("expr", show expr)]
