data Operator = Sum | Minus | Mul | Div

optochar :: Operator -> Char
optochar Sum = '+'
optochar Minus = '-'
optochar Mul = '*'
optochar Div = '/'

main = print $ optochar Div
