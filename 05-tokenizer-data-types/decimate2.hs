decimate :: [a] -> [a]
decimate (a:_:rest) = a : decimate rest
decimate (a:_) = [a]
decimate _ = []

main = do
   print (decimate [1, 2, 3, 4, 5])
   print (decimate [1, 2, 3, 4])
