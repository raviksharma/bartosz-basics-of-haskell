triples =
  [(x, y, z) | z <- [1..]
             , x <- [1..z]
             , y <- [x..z]
             , x * x + y * y == z * z]

main = print $ take 4 triples
