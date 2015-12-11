

module Environment 
    (Env,
    unEnv,
    emptyEnv,
    lookupInEnv,
    insertInEnv,
    removeInEnv) 
    where

import Template (render)

newtype Env a = Env {unEnv::[(String, a)]}

emptyEnv :: Env a
emptyEnv = Env []

lookupInEnv :: String -> Env a -> a
lookupInEnv key = helper . lookup key . unEnv
    where helper (Just n) = n

insertInEnv :: (String, a) -> Env a -> Env a
insertInEnv (n, a) env = Env $ insert' (n, a) (unEnv env)
    where insert' (n, a) ((n', a'):env) = 
              if n == n'
              then (n, a):env
              else (n', a'):(insert' (n, a) env)
          insert' (n, a) [] = [(n, a)]

instance Show a => Show (Env a) where
    show = unlines . map show' . unEnv
        where show' (n, a) = render "#{n} = #{a}" [("n", n), ("a", show a)]

removeInEnv :: String -> Env a -> Env a
removeInEnv n env = Env $ remove' n (unEnv env)
    where remove' n ((n', a'):env) = 
            if n == n'
            then env
            else (n', a'):(remove' n env)
          remove' _ [] = []
