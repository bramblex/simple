

module Environment 
    (Env,
    unEnv,
    inEnv,
    isBound,
    bound,
    emptyEnv,
    lookupInEnv,
    insertInEnv,
    removeInEnv) 
    where

import Template (render)

data Env a = Env {unEnv::[(String, a)], unBound::[String]}
    deriving(Read, Eq)

emptyEnv :: Env a
emptyEnv = Env [] []

inEnv :: String -> Env a -> Bool
inEnv name = any ((==name) . fst) . unEnv

isBound :: String -> Env a -> Bool
isBound name = any (==name) . unBound

bound :: String -> Env a -> Env a
bound name (Env env bound) = Env env (name:bound) 

lookupInEnv :: String -> Env a -> a
lookupInEnv name = helper . lookup name . unEnv
    where helper (Just n) = n
          helper (Nothing) = error $ "can not found value in enviroment " ++ name

insertInEnv :: (String, a) -> Env a -> Env a
insertInEnv (n, a) (Env env bound) = Env (insert' (n, a) (env)) (bound)
    where insert' (n, a) ((n', a'):env) = 
              if n == n'
              then (n, a):env
              else (n', a'):(insert' (n, a) env)
          insert' (n, a) [] = [(n, a)]

instance Show a => Show (Env a) where
    show = unlines . map show' . unEnv
        where show' (n, a) = render "#{n} = #{a}" [("n", n), ("a", show a)]

removeInEnv :: String -> Env a -> Env a
removeInEnv n (Env env bound) = Env (remove' n (env)) bound
    where remove' n ((n', a'):env) = 
            if n == n'
            then env
            else (n', a'):(remove' n env)
          remove' _ [] = []
