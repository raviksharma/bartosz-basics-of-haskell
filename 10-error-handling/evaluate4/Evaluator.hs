module Evaluator (evaluate, Evaluator(..)) where

import Lexer
import Parser
import qualified Data.Map as M

import Control.Applicative
import Control.Monad (liftM, ap)

newtype Evaluator a = Ev (Either String a)

instance Functor Evaluator where
  fmap = liftM

instance Applicative Evaluator where
  pure  = return
  (<*>) = ap

instance Monad Evaluator where
    (Ev ev) >>= k =
        case ev of
          Left msg -> Ev (Left msg)
          Right v -> k v
    return v = Ev (Right v)
    fail msg = Ev (Left msg)

type SymTab = M.Map String Double

evaluate :: Tree -> SymTab -> Evaluator (Double, SymTab)

evaluate (SumNode op left right) symTab = do
    (lft, symTab')  <- evaluate left symTab
    (rgt, symTab'') <- evaluate right symTab'
    case op of 
        Plus  -> return (lft + rgt, symTab'')
        Minus -> return (lft - rgt, symTab'')

evaluate (ProdNode op left right) symTab = do
    (lft, symTab')  <- evaluate left symTab
    (rgt, symTab'') <- evaluate right symTab'
    case op of
        Times -> return (lft * rgt, symTab)
        Div   -> return (lft / rgt, symTab)

evaluate (UnaryNode op tree) symTab = do
    (x, symTab') <- evaluate tree symTab
    case op of
        Plus  -> return ( x, symTab')
        Minus -> return (-x, symTab')

evaluate (NumNode x) symTab = return (x, symTab)

evaluate (VarNode str) symTab = lookUp str symTab

evaluate (AssignNode str tree) symTab = do
    (v, symTab')  <- evaluate tree symTab
    (_, symTab'') <- addSymbol str v symTab'
    return (v, symTab'')

lookUp :: String -> SymTab -> Evaluator (Double, SymTab)
lookUp str symTab = 
    case M.lookup str symTab of
      Just v  -> return (v, symTab)
      Nothing -> fail ("Undefined variable " ++ str)

addSymbol :: String -> Double -> SymTab -> Evaluator ((), SymTab)
addSymbol str val symTab = 
    let symTab' = M.insert str val symTab
    in return ((), symTab')
