-- Ex 4. Express the fish operator for standard lists considering the non-deterministic function interpretation (the solution is totally unsurprising)

import Data.Char

(>=>) :: (a -> [b]) -> (b -> [c]) -> (a -> [c])
f >=> g = \x -> concat (map g (f x))

modCase c = [toLower c, toUpper c]
camelize = modCase >=> modCase

main = print $ fmap camelize "Hump"
