data Operator = Plus | Minus | Times | Div
    deriving (Show, Eq)

opToStr :: Operator -> String
opToStr Plus  = "+"
opToStr Minus = "-"
opToStr Times = "*"
opToStr Div   = "/"

data Token = TokOp Operator
           | TokIdent String
           | TokNum Int
    deriving (Show, Eq)

showContent :: Token -> String
showContent (TokOp op) = opToStr op
showContent (TokIdent str) = str
showContent (TokNum i) = show i

token :: Token
token = TokIdent "x"

token2 :: Token
token2 = TokOp Plus

main = do
    putStrLn $ showContent token
    print token
    putStrLn $ showContent token2
    print token2
    putStrLn $ showContent (TokNum 5)
