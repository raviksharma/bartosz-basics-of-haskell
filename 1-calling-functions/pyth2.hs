-- Add parentheses and commas to code below to make it compile. You may reduce the number of parentheses if you take into account that the comma inside a tuple has lower precedence than arithmetic operators.
--
pyth' (a, b) = a * a + b * b

-- main = print $ pyth' 3 * 2 pyth' -1 8
main = print $ pyth' (3 * 2,  pyth' (-1, 8))
