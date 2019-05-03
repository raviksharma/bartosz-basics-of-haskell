data List a = Cons a (List a) | Empty

sumLst :: (List Int) -> Int
sumLst (Cons i rest) = i + sumLst rest
sumLst Empty = 0

sumDoubles :: (List Double) -> Double
sumDoubles (Cons i rest) = i + sumDoubles rest
sumDoubles Empty = 0

lst = Cons 2 (Cons 4 (Cons 6 Empty))
lst2 = Cons 2.0 (Cons 4 (Cons 6 Empty))

main = do
   print (sumLst lst)
   print (sumLst Empty)
   print (sumDoubles lst2)
