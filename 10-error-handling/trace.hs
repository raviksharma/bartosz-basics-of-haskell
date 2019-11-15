-- Ex 2. Define a monad instance for Trace (no need to override fail). The idea is to create a trace of execution by sprinkling you code with calls to put. The result of executing this code should look something like this:
--
-- ["fact 3","fact 2","fact 1","fact 0"]
-- 6
-- Hint: List concatenation is done using ++ (we've seen it used for string concatenation, because String is just a list of Char).

import Control.Applicative
import Control.Monad (liftM, ap)

newtype Trace a = Trace ([String], a)

instance Functor Trace where
  fmap = liftM

instance Applicative Trace where
  pure  = return
  (<*>) = ap

instance Monad Trace where
    return x = Trace ([], x)
    (Trace (lst, x)) >>= k =
        let Trace (lst', y) = k x
        in Trace (lst ++ lst', y)

put :: Show a => String -> a -> Trace ()
put msg v = Trace ([msg ++ " " ++ show v], ())

fact :: Integer -> Trace Integer
fact n = do
   put "fact" n
   if n == 0
       then return 1
       else do
           m <- fact (n - 1)
           return (n * m)

main = let Trace (lst, m) = fact 3
       in do
           print lst
           print m
