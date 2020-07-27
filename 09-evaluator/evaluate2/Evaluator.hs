module Evaluator (evaluate) where

import Lexer
import Parser
import qualified Data.Map as M

type SymTab = M.Map String Double

evaluate :: Tree -> SymTab -> (Double, SymTab)

evaluate (SumNode op left right) symTab = 
    let (lft, symTab') = evaluate left symTab
        (rgt, symTab'') = evaluate right symTab'
    in
        case op of
          Plus  -> (lft + rgt, symTab'')
          Minus -> (lft - rgt, symTab'')

evaluate (ProdNode op left right) symTab = 
    let (lft, symTab') = evaluate left symTab
        (rgt, symTab'') = evaluate right symTab'
    in
        case op of
          Times -> (lft * rgt, symTab)
          Div   -> (lft / rgt, symTab)

evaluate (UnaryNode op tree) symTab =
    let (x, symTab') = evaluate tree symTab
    in case op of
         Plus  -> (x, symTab')
         Minus -> (-x, symTab')

evaluate (NumNode x) symTab = (x, symTab)

evaluate (VarNode str) symTab = lookUp str symTab

evaluate (AssignNode str tree) symTab = 
    let (v, symTab')  = evaluate tree symTab
        (_, symTab'') = addSymbol str v symTab'
    in (v, symTab'')

lookUp :: String -> SymTab -> (Double, SymTab)
lookUp str symTab = 
    case M.lookup str symTab of
      Just v -> (v, symTab)
      Nothing -> error $ "Undefined variable " ++ str

addSymbol :: String -> Double -> SymTab -> ((), SymTab)
addSymbol str val symTab = 
    let symTab' = M.insert str val symTab
    in ((), symTab')
