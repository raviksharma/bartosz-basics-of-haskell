-- Try to remove as many parentheses as you can using $ signs
pyth a b = a * a + b * b

--main = do print (sqrt (pyth 3 ((-1) - 3)))
main = do print $ sqrt $ pyth 3 $ -1 - 3
