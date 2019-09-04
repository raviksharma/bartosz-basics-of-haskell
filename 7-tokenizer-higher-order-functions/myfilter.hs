import Data.Char
import Prelude hiding (filter)

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter f (a: rest) = if f a
                     then a : filter f rest
                     else filter f rest

main = print $ filter isDigit "a1b2c3" 
