
module Template (render) where

render :: String -> [(String, String)] -> String
render ('#':'{':xs) kvs = helper (lookup key kvs) ++ render next kvs
    where (key, next) = splitAtEnd ("", xs)
          splitAtEnd (key, ('}':xs)) = (key, xs)
          splitAtEnd (key, (x:xs)) = splitAtEnd (key++[x], xs)
          helper (Just n) = n
render (x:xs) kvs = x : render xs kvs
render "" kvs = ""
