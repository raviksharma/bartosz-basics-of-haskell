-- Add parentheses to the code below to make it compile 
pyth a b = a * a + b * b

-- main = print $ pyth 3 * 2 pyth -1 8

main = print $ pyth (3 * 2) (pyth (-1) 8)
