sumLst :: [Int] -> Int
sumLst (i : rest) = i + sumLst rest
sumLst [] = 0

lst = [2, 4, 6]

main = do
   print (sumLst lst)
   print (sumLst [])
