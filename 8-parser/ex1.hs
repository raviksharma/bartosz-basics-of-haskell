-- The shape of a binary tree may be encoded using matching pairs of parentheses. The string of parentheses obtained this way matches the following grammar:
--
-- Root  <- Par
-- Expr  <- Par Par
-- Par   <- '(' Expr ')'
--        | '(' ')'
--

data Token = TokLParen | TokRParen | TokEnd
    deriving (Show, Eq)

lookAhead :: [Char] -> Token
lookAhead [] = TokEnd
lookAhead (c:cs)| c == '(' = TokLParen
                | c == ')' = TokRParen
                | otherwise = error $ "Bad input: " ++ (c:cs)

accept :: [Char] -> [Char]
accept [] = error "Nothing to accept"
accept (c:cs) = cs

data Tree = Node Tree Tree | Leaf
    deriving Show

root, expr, par :: [Char] -> (Tree, [Char])

root = par

expr toks = 
    let (p, toks')   = par toks 
        (p', toks'') = par toks'
    in (Node p p', toks'')
    
par toks = 
    case lookAhead toks of
      TokLParen ->
        case lookAhead (accept toks) of
          TokRParen -> (Leaf, accept (accept toks))
          _ -> let (e, toks') = expr (accept toks) 
               in  if lookAhead toks' == TokRParen
                   then (e, accept toks')
                   else error $ "Missing closing paren in: " ++ show toks'
      _ -> error $ "Bad expression: " ++ show toks

parse str = let (tree, str') = root str
            in
                if null str' 
                then tree 
                else error $ "Unconsumed string " ++ str'

main = print $ parse "(()(()()))"

