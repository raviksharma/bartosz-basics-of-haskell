import Data.Char

data Operator = Plus | Minus | Times | Div
    deriving (Show, Eq)

data Token = TokOp Operator
           | TokIdent String
           | TokNum Int
           | TokSpace
    deriving (Show, Eq)

operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '*' = Times
           | c == '/' = Div

tokenize :: String -> [Token]
tokenize = map tokenizeChar

tokenizeChar :: Char -> Token
tokenizeChar c | elem c "+-*/" = TokOp (operator c)
               | isDigit c  = TokNum (digitToInt c)
               | isAlpha c  = TokIdent [c]
               | isSpace c  = TokSpace
               | otherwise  = error $ "Cannot tokenize " ++ [c]

deSpace :: [Token] -> [Token]
deSpace = filter (\t -> t /= TokSpace) 

main = print $ deSpace $ tokenize " 1 + 4 / x "
