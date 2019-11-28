f [] = []
f (p:xs) = f [x | x <- xs, x < p] 
        ++   [p]
        ++ f [x | x <- xs, x >= p]

main = print $ f [2, 5, 1, 3, 4]
