-- The accumulator in foldl can also be a list. With this in mind, implement function rev that reverses a list.
rev :: [a] -> [a]
rev = foldl (\acc a -> a : acc) []

main = print $ rev "spot on"
