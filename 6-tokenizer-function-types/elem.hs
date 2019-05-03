isDigit :: Char -> Bool
isDigit c = elem c "0123456789"

main = print $ isDigit '3'
