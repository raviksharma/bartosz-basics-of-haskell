--Use cat from previous exercise and currying to define a function pig that prepends "pig" to any string.

cat :: [a] -> [a] -> [a]
cat [] j = j
cat (i : rest) j = i : cat rest j

pig :: String -> String
pig = cat "pig"

main = putStrLn $ pig "sty"
