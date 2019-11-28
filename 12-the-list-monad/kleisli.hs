(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
g <=< f = \x -> f x >>= g

f x = [x, x + 1]
g x = [x * x]

test = g <=< f

main = print $ test 7
