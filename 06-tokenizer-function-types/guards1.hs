data Operator = Plus | Minus | Times | Div
    deriving (Show, Eq)

operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '*' = Times
           | c == '/' = Div

main = do
   print $ operator '*'
   print $ operator '+'
