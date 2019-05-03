data List = Cons Int List | Empty

sumLst :: List -> Int
sumLst (Cons i rest) = i + sumLst rest
sumLst Empty = 0

lst = Cons 2 (Cons 4 (Cons 6 Empty))

main = do
   print (sumLst lst)
   print (sumLst Empty)
