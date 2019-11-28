-- Ex 2. Define functions listBind and listReturn for regular lists in a way analogous to >>= and return for our private lists (again, it's pretty simple):

import Data.Char

listBind :: [a] -> (a -> [b]) -> [b]
listBind xs k = concat (map k xs)

listReturn :: a -> [a]
listReturn x = [x]

neighbors x = [x - 1, x, x + 1]

main = do
    print $ listBind [10, 20, 30] neighbors
    print $ listBind "string" (listReturn . ord)
