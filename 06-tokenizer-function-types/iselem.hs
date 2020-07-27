isElem c (d : rest) = if c == d 
                      then True 
                      else isElem c rest
isElem _ [] = False

main = do
    print $ isElem '3' "abc"
    print $ isElem '3' "123"
