import Data.Char
import qualified Data.Map as M

data Operator = Plus | Minus | Times | Div
    deriving (Show, Eq)

data Token = TokOp Operator
           | TokAssign
           | TokLParen
           | TokRParen
           | TokIdent String
           | TokNum Double
           | TokEnd
    deriving (Show, Eq)

operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '*' = Times
           | c == '/' = Div

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c : cs) 
    | elem c "+-*/" = TokOp (operator c) : tokenize cs
    | c == '='  = TokAssign : tokenize cs
    | c == '('  = TokLParen : tokenize cs
    | c == ')'  = TokRParen : tokenize cs
    | isDigit c = number c cs
    | isAlpha c = identifier c cs
    | isSpace c = tokenize cs
    | otherwise = error $ "Cannot tokenize " ++ [c]

identifier :: Char -> String -> [Token]
identifier c cs = let (name, cs') = span isAlphaNum cs in
                  TokIdent (c:name) : tokenize cs'

number :: Char -> String -> [Token]
number c cs = 
   let (digs, cs') = span isDigit cs in
   TokNum (read (c : digs)) : tokenize cs'

---- parser ----

data Tree = SumNode Operator Tree Tree
          | ProdNode Operator Tree Tree
          | AssignNode String Tree
          | UnaryNode Operator Tree
          | NumNode Double
          | VarNode String
    deriving Show

lookAhead :: [Token] -> Token
lookAhead [] = TokEnd
lookAhead (t:ts) = t

accept :: [Token] -> [Token]
accept [] = error "Nothing to accept"
accept (t:ts) = ts

expression :: [Token] -> (Tree, [Token])
expression toks = 
   let (termTree, toks') = term toks
   in
      case lookAhead toks' of
         (TokOp op) | elem op [Plus, Minus] -> 
            let (exTree, toks'') = expression (accept toks') 
            in (SumNode op termTree exTree, toks'')
         TokAssign ->
            case termTree of
               VarNode str -> 
                  let (exTree, toks'') = expression (accept toks') 
                  in (AssignNode str exTree, toks'')
               _ -> error "Only variables can be assigned to"
         _ -> (termTree, toks')

term :: [Token] -> (Tree, [Token])
term toks = 
   let (facTree, toks') = factor toks
   in
      case lookAhead toks' of
         (TokOp op) | elem op [Times, Div] ->
            let (termTree, toks'') = term (accept toks') 
            in (ProdNode op facTree termTree, toks'')
         _ -> (facTree, toks')

factor :: [Token] -> (Tree, [Token])
factor toks = 
   case lookAhead toks of
      (TokNum x)     -> (NumNode x, accept toks)
      (TokIdent str) -> (VarNode str, accept toks)
      (TokOp op) | elem op [Plus, Minus] -> 
            let (facTree, toks') = factor (accept toks) 
            in (UnaryNode op facTree, toks')
      TokLParen      -> 
         let (expTree, toks') = expression (accept toks)
         in
            if lookAhead toks' /= TokRParen 
            then error "Missing right parenthesis"
            else (expTree, accept toks')
      _ -> error $ "Parse error on token: " ++ show toks

parse :: [Token] -> Tree
parse toks = let (tree, toks') = expression toks
             in
               if null toks' 
               then tree
               else error $ "Leftover tokens: " ++ show toks'

---- evaluator ----
-- show

type SymTab = M.Map String Double

lookUp :: String -> SymTab -> Either String (Double, SymTab)
lookUp str symTab = 
    case M.lookup str symTab of
      Just v  -> Right (v, symTab)
      Nothing -> Left ("Undefined variable " ++ str)

addSymbol :: String -> Double -> SymTab -> Either String ((), SymTab)
addSymbol str val symTab = 
    let symTab' = M.insert str val symTab
    in Right ((), symTab')

evaluate :: Tree -> SymTab -> Either String (Double, SymTab)

evaluate (SumNode op left right) symTab = 
    case evaluate left symTab of
    Left msg -> Left msg
    Right (lft, symTab') ->
        case evaluate right symTab' of
        Left msg -> Left msg
        Right (rgt, symTab'') ->
            case op of
            Plus  -> Right (lft + rgt, symTab'')
            Minus -> Right (lft - rgt, symTab'')

evaluate (ProdNode op left right) symTab = 
    case evaluate left symTab of
    Left msg -> Left msg
    Right (lft, symTab') ->
        case evaluate right symTab' of
        Left msg -> Left msg
        Right (rgt, symTab'') ->
            case op of
            Times -> Right (lft * rgt, symTab)
            Div   -> Right (lft / rgt, symTab)

evaluate (UnaryNode op tree) symTab =
    case evaluate tree symTab of
    Left msg -> Left msg
    Right (x, symTab') ->
        case op of
        Plus  -> Right ( x, symTab')
        Minus -> Right (-x, symTab')

evaluate (NumNode x) symTab = Right (x, symTab)

evaluate (VarNode str) symTab = lookUp str symTab

evaluate (AssignNode str tree) symTab = 
    case evaluate tree symTab of
    Left msg -> Left msg
    Right (v, symTab') ->
        case addSymbol str v symTab' of
        Left msg -> Left msg
        Right (_, symTab'') -> Right (v, symTab'')

main = do
   loop (M.fromList [("pi", pi)])

loop symTab = do
   str <- getLine
   if null str
   then
      return ()
   else
      let toks = tokenize str
          tree = parse toks
      in
          case evaluate tree symTab of
          Left msg -> do
              putStrLn $ "Error: " ++ msg
              loop symTab -- use old symTab
          Right (v, symTab') -> do
              print v
              loop symTab'
