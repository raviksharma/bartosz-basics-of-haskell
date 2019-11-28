-- Ex 3. Implement the other fish operator that composes from left to right:

(>=>) :: Monad m =>  (a -> m b) -> (b -> m c) -> (a -> m c)
f >=> g = \x -> f x >>= g

f x = [x, x + 1]
g x = [x * x]

test1 = f >=> g
test2 = f >=> g >=> g
test3 = g >=> f >=> f

main = do
    print $ test1 3
    print $ test2 3
    print $ test3 3
