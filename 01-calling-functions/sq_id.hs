sq x = x * x
main = print $ (sq . sqrt . id) 256
