data List = Cons Int List | Empty

singleton :: List -> Bool
singleton (Cons _ Empty) = True
singleton _ = False

main = do
   print $ singleton Empty
   print $ singleton $ Cons 2 Empty
   print $ singleton $ Cons 3 $ Cons 4 Empty
