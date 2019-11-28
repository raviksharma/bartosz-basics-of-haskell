pairs l1 l2 = do
    x <- l1
    y <- l2
    return (x, y)

main = do
    print $ pairs [1, 2, 3] "abc"
    print $ [(x, y) | x <- [1..3], y <- "abc"]

