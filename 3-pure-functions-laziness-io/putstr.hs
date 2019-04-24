putStrLn' str = do
    putStr str
    putChar '\n'

main = do
    putStrLn' "First line"
    putStrLn' "Second line"
