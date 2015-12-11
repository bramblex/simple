
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Lambda
import Parser
import Environment

import Control.Monad.Reader
import Control.Monad.Writer

type Configuration = (Expr, Env Expr)
type Output = [Expr]
type Machine a = ReaderT Configuration (Writer Output) a
newtype Comp a = Comp {unComp:: Machine a}
    deriving(Functor, Applicative, Monad, MonadReader Configuration, MonadWriter Output)

evalComp :: Comp Expr
evalComp = do
    (expr, env) <- ask
    tell [expr]
    case reducible expr of
        False -> return expr
        True -> local (const $ reduce expr env) evalComp

runMachine :: Configuration -> (Expr, Output)
runMachine = runWriter . runReaderT (unComp evalComp)

bindEnv :: Program -> Env Expr
bindEnv = foldl bind' emptyEnv
    where bind' env (Binding name expr) = insertInEnv (name, expr) env

readProgram :: String -> Program
readProgram = helper . parseProgram
    where helper (Right a) = a

runProgram :: Env Expr -> (Expr, Output)
runProgram env = runMachine (lookupInEnv "main" env, env) 

main :: IO ()
main = do 
    code <- getContents
    let env =  bindEnv . readProgram $ code
    putStrLn "Program: "
    putStr . unlines . map ("  "++) . lines . show $ env
    putStrLn "Main: "
    let (expr, output) = runProgram env 
    mapM_ (putStrLn . ("  main = "++) . show) $ output

