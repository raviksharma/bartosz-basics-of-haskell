-- Ex 3. Instead of deriving Show, define explicit instances of the Show typeclass for Operator and Tree such that expr is displayed as:
--
-- x = (13.0 - 1.0) / y
-- It's enough that you provide the implementation of the show function in the instance declaration. This function should take an Operator (or a Tree) and return a string.

data Operator = Plus | Minus | Times | Div

data Tree = SumNode Operator Tree Tree
          | ProdNode Operator Tree Tree
          | AssignNode String Tree
          | UnaryNode Operator Tree
          | NumNode Double
          | VarNode String

instance Show Operator where
    show Plus  = " + "
    show Minus = " - "
    show Times = " * "
    show Div   = " / "

instance Show Tree where
    show (SumNode op lft rgt) = "(" ++ show lft ++ show op ++ show rgt ++ ")"
    show (ProdNode op lft rgt) = show lft ++ show op ++ show rgt
    show (AssignNode str tree) = str ++ " = " ++ show tree
    show (UnaryNode op tree) = show op ++ show tree
    show (NumNode x) = show x
    show (VarNode str) = str

expr = AssignNode "x" (ProdNode Div (SumNode Minus (NumNode 13) (NumNode 1)) (VarNode "y"))

main = print expr
