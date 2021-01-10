isElem c (d : rest) = if c == d 
                      then True 
                      else isElem c rest
isElem _ [] = False

is3elem = isElem '3'

main = print $ is3elem "123"
