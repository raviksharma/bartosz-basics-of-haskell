fib :: Int -> Int
fib n = if n > 2 then fib (n - 1) + fib (n - 2) else 1

main = print (fib 8)
