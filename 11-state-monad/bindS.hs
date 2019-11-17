data Operator = Plus | Minus
data Tree = UnaryNode Operator Tree
type SymTab = ()
-- show
type Evaluator a = SymTab -> (a, SymTab)

returnS :: a -> Evaluator a
returnS x = \symTab -> (x, symTab)

bindS :: Evaluator a
      -> (a -> Evaluator b)
      -> Evaluator b
bindS act k =
    \symTab ->
        let (x, symTab') = act symTab
            act' = k x
        in 
            act' symTab'

evaluate :: Tree -> (SymTab -> (Double, SymTab))
evaluate (UnaryNode op tree) =
    bindS (evaluate tree)
          (\x -> case op of
                   Plus  -> returnS x
                   Minus -> returnS (-x))

main = putStrLn "It type checks!"
