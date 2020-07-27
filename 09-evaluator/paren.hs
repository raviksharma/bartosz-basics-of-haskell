-- Implement function paren that takes an expression tree and turns it into a string with fully parenthesized expression. For instance, when acting on testExpr it should produce the string (x = ((2.0 * (y = 5.0)) + 3.0))

data Operator = Plus | Minus | Times | Div
    deriving (Show, Eq)

data Tree = SumNode Operator Tree Tree
          | ProdNode Operator Tree Tree
          | AssignNode String Tree
          | UnaryNode Operator Tree
          | NumNode Double
          | VarNode String
    deriving Show

paren :: Tree -> String
paren (SumNode op left right) =
    case op of
      Plus  -> bin " + " left right
      Minus -> bin " - " left right

paren (ProdNode op left right) =
    case op of
      Times -> bin " * " left right
      Div   -> bin " / " left right

paren (AssignNode var tree) =
    let treeS = paren tree 
    in "(" ++ var ++ " = " ++ treeS ++ ")"

paren (UnaryNode op tree) =
    let treeS = paren tree
        opS = case op of 
                Plus -> " +"
                Minus -> " -"
    in "(" ++ opS ++ treeS ++ ")"

paren (NumNode x) = show x

paren (VarNode var) = var

bin :: String -> Tree -> Tree -> String
bin op left right =
    let leftS = paren left
        rightS = paren right
    in
        "(" ++ leftS ++ op ++ rightS ++ ")"

-- x = 2 * (y = 5) + 3
testExpr = AssignNode "x" (SumNode Plus 
                             (ProdNode Times 
                               (NumNode 2.0) 
                               (AssignNode "y" (NumNode 5)))
                             (NumNode 3))

main = print $ paren testExpr

