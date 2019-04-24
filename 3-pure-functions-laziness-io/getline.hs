-- exercise to take the input string from the user
putQStrLn str = do
    putChar '"'
    putStr str
    putChar '"'
    putChar '\n'

main = do
    putStrLn "Enter text:"
    str <- getLine
    putQStrLn str
