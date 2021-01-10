import Data.Char -- for the example
import Prelude hiding (map)
-- show
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (a : as) = f a : map f as

main = print $ map toUpper "hello world!"
