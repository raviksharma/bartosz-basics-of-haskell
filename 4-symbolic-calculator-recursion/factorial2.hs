-- The evaluation of factorial starts returning incorrect results right about n = 21 because of the  Int overflow. Try implementing a version that uses the infinite precision Integer instead of  Int.
--
fact :: Int -> Int
fact n = if n > 0 then n * fact (n - 1) else 1

fullFact :: Integer -> Integer
fullFact n =  if n > 0 then n * fullFact (n - 1) else 1

main = do
    print (fact 23)
    print (fullFact 23)
