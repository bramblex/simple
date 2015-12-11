
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Control.Monad.Trans
import Control.Monad.Trans.Writer

type Output = [Char]
type Test a = WriterT Output (Writer Output) a

newtype TestComp a = TestComp {unTestComp::Test a}
    deriving(Functor, Applicative, Monad)

runTestComp :: TestComp a -> ((a, Output), Output)
runTestComp = runWriter . runWriterT . unTestComp

test :: TestComp ()
test = TestComp $ do 
    tell "aaa"
    tell "bbb"
    tell "ccc"
    lift $ tell "aaa"
