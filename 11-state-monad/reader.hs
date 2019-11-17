-- Ex 1. Define the reader monad. It's supposed to model computations that have access to some read-only environment. In imperative code such environment is often implemented as a global object. In functional languages we need to pass it as an argument to every function that might potentially need access to it. The reader monad hides this process.

import Control.Applicative
import Control.Monad (liftM, ap)

instance Functor (Reader e) where
  fmap = liftM

instance Applicative (Reader e) where
  pure  = return
  (<*>) = ap

newtype Reader e a = Reader (e -> a)

reader :: (e -> a) -> Reader e a
reader f = Reader f

runReader :: Reader e a -> e -> a
runReader (Reader act) env = act env

ask :: Reader e e
ask = reader (\e -> e)

instance Monad (Reader e) where
    return x = reader (\_ -> x)
    rd >>= k = reader $ \env ->
                           let x = runReader rd env
                               act' = k x
                           in runReader act' env

type Env = Reader String
-- curried version of
-- type Env a = Reader String a

test :: Env Int
test = do
    s <- ask
    return $ read s + 5

main = print $ runReader test "13"
