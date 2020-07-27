main :: IO ()
main = do
    line <- getLine
    putStrLn line
    main
