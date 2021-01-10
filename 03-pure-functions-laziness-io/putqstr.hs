-- Define a function putQStrLn that outputs a string surrounded by quotes, '"'
putQStrLn str = do
    putChar '"'
    putStr str
    putChar '"'
    putChar '\n'

main = putQStrLn "You can quote me."
