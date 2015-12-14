
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lambda where

import Data.Char

import Environment
import Template

data Expr = Var String | Lam String Expr | App Expr Expr
          deriving (Eq)

data Binding = Binding String Expr
    deriving (Eq)

type Program = [Binding]

class Reducible a where
    reducible :: a -> Env a -> Bool
    reducible = reducibleLazy

    reducibleLazy :: a -> Env a -> Bool
    reducibleLazy = reducible

    reducibleImmediately:: a -> Env a -> Bool
    reducibleImmediately = reducible

    reduce :: a -> Env a -> (a, Env a)
    reduce = reduceLazy

    reduceLazy :: a -> Env a -> (a, Env a)
    reduceLazy = reduce

    reduceImmediately :: a -> Env a -> (a, Env a)
    reduceImmediately = reduce

class Callable a where
    callable :: a -> Bool
    replace :: a -> String -> a -> a 

instance Callable Expr where
    callable (Lam _ _) = True 
    callable _ = False

    replace self@(Var v) name expr = 
        if v == name then expr else self
    replace self@(Lam v e) name expr@(Var var_name) = 
        let new_name = v ++ "'"
        in if v == var_name
           then replace (Lam new_name (replace e v (Var new_name))) name expr
           else Lam v (replace e name expr)
    replace self@(Lam v e) name expr =
        if v == name 
        then self
        else Lam v (replace e name expr)

    replace self@(App e1 e2) name expr =
        App (replace e1 name expr) (replace e2 name expr)

instance Reducible Expr where
    reducibleLazy (App e1 e2) env = (reducibleLazy e1 env) || (callable e1)
    reducibleLazy (Var name) env = not (isBound name env) && inEnv name env
    reducibleLazy _ _ = False

    reduceLazy (App e1 e2) env 
        | reducibleLazy e1 env = (App (fst $ reduceLazy e1 env) e2, env)
        {-| reducible e2 env = (App e1 (fst $ reduce e2 env), env)-}
        | otherwise = (replace' e1 e2, env)
            where replace' (Lam name expr) e2 = replace expr name e2
    reduceLazy (Var name) env = (lookupInEnv name env, env)

    reducibleImmediately (App e1 e2) env = 
        (reducibleImmediately e1 env) ||
        (reducibleImmediately e2 env) ||
        (callable e1)
    reducibleImmediately (Var name) env = not (isBound name env) && inEnv name env
    reducibleImmediately (Lam _ expr) env = reducibleImmediately expr env

    reduceImmediately (App e1 e2) env 
        | reducibleImmediately e1 env =
            (App (fst $ reduceImmediately e1 env) e2, env)
        | reducibleImmediately e2 env = 
            (App e1 (fst $ reduceImmediately e2 env), env)
        | otherwise = (replace' e1 e2, env)
            where replace' (Lam name expr) e2 = replace expr name e2
    reduceImmediately (Lam name expr) env = 
        (Lam name (fst $ reduceImmediately expr (bound name env)), env)
    reduceImmediately (Var name) env = (lookupInEnv name env, env)

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
