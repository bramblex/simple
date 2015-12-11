
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Control.Monad.Writer
import Control.Monad.Reader

data Op = Add | Minus | Mul | Div | Equal | LessThan | MoreThan | Or | And
        deriving (Eq, Read)

data Expr = Number Float | Boolean Bool | Variable String | Expr Op Expr Expr
          deriving (Eq, Read)

data Statement = DoNothing
               | Assign String Expr
               | IfElse Expr Statement Statement
               | While Expr Statement
               | Sequence Statement Statement
               deriving (Eq, Read)


class Reducible a where
    reducible :: a -> Bool
    reduce :: a -> Env Expr -> (a, Env Expr)

instance Reducible Expr where
    reducible (Number _) = False
    reducible (Boolean _) = False
    reducible _  = True

    reduce (Variable name) env = (lookupInEnv name env, env)
    reduce (Expr op left right) env 
        | reducible left = (Expr op (fst $ reduce left env) right, env)
        | reducible right = (Expr op left (fst $ reduce right env), env)
        | otherwise = (reduce' op left right, env)
            where 
                reduce' op (Number l) (Number r) = case op of
                    Add -> Number (l+r)
                    Minus -> Number (l-r)
                    Mul -> Number (l*r)
                    Div -> Number (l/r)

                    Equal -> Boolean (l==r)
                    LessThan -> Boolean (l<r)
                    MoreThan -> Boolean (l>r)

                reduce' op (Boolean l) (Boolean r) = case op of
                    Equal -> Boolean (l==r)
                    And -> Boolean (l&&r)
                    Or -> Boolean (l||r)

instance Reducible Statement where
    reducible DoNothing = False
    reducible _ = True

    reduce (Assign name expr) env 
        | reducible expr = (Assign name (fst $ reduce expr env), env)
        | otherwise = (DoNothing, insertInEnv (name, expr) env)

    reduce (IfElse condition consequence alternative) env
        | reducible condition = (IfElse (fst $ reduce condition env) consequence alternative, env)
        | otherwise = case condition of
            Boolean True -> (consequence, env)
            Boolean False -> (alternative, env)

    reduce self@(While condition body) env = 
        (IfElse condition (Sequence body self) DoNothing, env)

    reduce (Sequence first second) env
        | reducible first = 
            let (first', env') = reduce first env
            in (Sequence first' second, env')
        | otherwise = (second, env)

type Program = Statement
type Configuration = (Program, Env Expr)
type OutPut = [Configuration]
type Machine a = ReaderT Configuration (Writer OutPut) a

newtype Comp a = Comp { unComp:: Machine a}
    deriving(Functor, Applicative, Monad, MonadReader Configuration, MonadWriter OutPut)

eval :: Comp ()
eval = do
    configuration@(statement, env) <- ask
    tell [configuration]
    case reducible statement of
        False -> return ()
        True -> local (const $ reduce statement env) eval

execMachine :: Configuration -> OutPut
execMachine = execWriter . runReaderT (unComp eval)

execProgram :: Program -> OutPut
execProgram program = execMachine (program, emptyEnv)

readProgram :: String -> Program
readProgram = read

main :: IO ()
main = interact $ unlines . map show . execProgram . readProgram

{-- #################################################### --}
instance Show Op where
    show op = case op of
        Add -> "+"
        Minus -> "-"
        Mul -> "×"
        Div -> "÷"
        Equal -> "=="
        LessThan -> "<"
        MoreThan -> ">"
        Or -> "||"
        And -> "&&"

instance Show Expr where
    show (Number n) =
        if n == fromInteger (round n) 
        then show (round n)
        else show n
    show (Boolean n) = show n
    show (Variable s) = s
    show (Expr op left right) = render "(#{left}#{op}#{right})" [("left", show left), ("right", show right), ("op", show op)]

instance Show Statement where
    show DoNothing = "do-nothing"
    show (Assign name expr) = name ++ "=" ++ show expr
    show (IfElse condition consequence alternative) =
        render "if (#{condition}) { #{consequence} } else { #{alternative} }" 
            [("condition", show condition),
            ("consequence", show consequence),
            ("alternative", show alternative)]
    show (While condition body) =
        render "while (#{condition}) { #{body} }"
            [("condition", show condition),
            ("body", show body)]
    show (Sequence first second) = 
        render "#{first}; #{second}"
            [("first", show first),
            ("second", show second)]

newtype Env a = Env {unEnv::[(String, a)]}
    deriving(Show)

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

{-instance Show a => Show (Env a) where-}
    {-show = unlines . map show' . unEnv-}
        {-where show' (n, a) = render "#{n} = #{a}" [("n", n), ("a", show a)]-}

render :: String -> [(String, String)] -> String
render ('#':'{':xs) kvs = helper (lookup key kvs) ++ render next kvs
    where (key, next) = splitAtEnd ("", xs)
          splitAtEnd (key, ('}':xs)) = (key, xs)
          splitAtEnd (key, (x:xs)) = splitAtEnd (key++[x], xs)
          helper (Just n) = n
render (x:xs) kvs = x : render xs kvs
render "" kvs = ""

