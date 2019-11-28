squares lst = do
    x <- lst
    return (x * x)

squares2 lst = lst >>= \x -> return (x * x)

squares3 lst = 
    concat $ fmap k lst
  where
    k = \x -> [x * x]

squares4 = fmap sq
    where sq x = x * x

main = do
    print $ squares [1, 2, 3]
    print $ squares2 [1, 2, 3]
    print $ squares3 [1, 2, 3]
    print $ squares4 [1, 2, 3]
