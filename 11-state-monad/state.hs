-- Ex 2. Use the State monad from Control.Monad.State to re-implement the evaluator.

import Data.Char
import qualified Data.Map as M
import Control.Monad.State

data Operator = Plus | Minus | Times | Div
    deriving (Show, Eq)

data Tree = SumNode Operator Tree Tree
          | ProdNode Operator Tree Tree
          | AssignNode String Tree
          | UnaryNode Operator Tree
          | NumNode Double
          | VarNode String
    deriving Show

type SymTab = M.Map String Double

type Evaluator a = State SymTab a

lookUp :: String -> Evaluator Double
lookUp str = do
    symTab <- get
    case M.lookup str symTab of
      Just v  -> return v
      Nothing -> error $ "Undefined variable " ++ str

addSymbol :: String -> Double -> Evaluator ()
addSymbol str val = do 
    symTab <- get
    put $ M.insert str val symTab
    return ()

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
    return v

expr = AssignNode "x" (ProdNode Times (VarNode "pi") 
                                (ProdNode Times (NumNode 4) (NumNode 6)))

main = print $ runState (evaluate expr) (M.fromList [("pi", pi)])
