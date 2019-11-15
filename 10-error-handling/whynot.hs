-- Ex 1. Define the whynot monad

import Control.Applicative
import Control.Monad (liftM, ap)

instance Functor WhyNot where
  fmap = liftM

instance Applicative WhyNot where
  pure  = return
  (<*>) = ap

data WhyNot a = Nah | Sure a
  deriving Show

instance Monad WhyNot where
   Sure x >>= k = k x
   Nah    >>= _ = Nah
   return x     = Sure x
   fail _       = Nah

safeRoot :: Double -> WhyNot Double
safeRoot x = 
    if x >= 0 then 
      return (sqrt x)
    else
      fail "Boo!"

test :: Double -> WhyNot Double
test x = do
   y <- safeRoot x
   z <- safeRoot (y - 4)
   w <- safeRoot z
   return w


main = do
    print $ test 9
    print $ test 400
