-- Implement the function decimate that skips every other element of a list.

decimate :: [a] -> [a]
decimate (i : j : rest) = i : decimate rest 
decimate (i : j) = [i] 
decimate [] = []

main = do
   print (decimate [1, 2, 3, 4, 5])
   print (decimate [1, 2, 3, 4])
