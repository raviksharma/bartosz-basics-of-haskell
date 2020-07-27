--Implement function cat that concatenates two lists.

cat :: [a] -> [a] -> [a]
cat [] j = j   
cat (i : rest) j = i : cat rest j

main = putStrLn $ cat "Hello " "World!"
