import Data.Char
import qualified Data.Map as M

import Control.Applicative
import Control.Monad (liftM, ap)

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

newtype Evaluator a = Ev (SymTab -> (a, SymTab))

instance Functor Evaluator where
  fmap = liftM

instance Applicative Evaluator where
  pure  = return
  (<*>) = ap

instance Monad Evaluator where
    (Ev act) >>= k = Ev $
        \symTab -> 
            let (x, symTab') = act symTab
                (Ev act') = k x
            in act' symTab'
    return x = Ev (\symTab -> (x, symTab))

lookUp :: String -> Evaluator Double
lookUp str = Ev $ \symTab ->
    case M.lookup str symTab of
      Just v  -> (v, symTab)
      Nothing -> error $ "Undefined variable " ++ str

addSymbol :: String -> Double -> Evaluator Double
addSymbol str val = Ev $ \symTab ->
    let symTab' = M.insert str val symTab
    in  (val, symTab')

evaluate :: Tree -> Evaluator Double

evaluate (SumNode op left right) = do
    lft <- evaluate left
    rgt <- evaluate right
    case op of
       Plus  -> return $ lft + rgt
       Minus -> return $ lft - rgt

evaluate (ProdNode op left right) = do
    lft <- evaluate left
    rgt <- evaluate right
    case op of
       Times -> return $ lft * rgt
       Div   -> return $ lft / rgt

evaluate (UnaryNode op tree) = do
    x <- evaluate tree 
    case op of
       Plus  -> return x
       Minus -> return (-x)

evaluate (NumNode x) = return x

evaluate (VarNode str) = lookUp str

evaluate (AssignNode str tree) = do
    v <- evaluate tree
    addSymbol str v

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
          Ev act = evaluate tree
          (val, symTab') = act symTab
      in do
          print val
          loop symTab'
