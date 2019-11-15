class Expr a

data Const   = Const Double
data Add a b = Add a b

instance Expr Const
instance (Expr a, Expr b) => Expr (Add a b)

class (Expr e) => Valuable e where
    evaluate :: e -> Double

instance Valuable Const where
    evaluate (Const x) = x
instance (Valuable a, Valuable b) => Valuable (Add a b) where
    evaluate (Add lft rgt) = evaluate lft + evaluate rgt

-- client code

data Mul a b = Mul a b

instance (Expr a, Expr b) => Expr (Mul a b)
instance (Valuable a, Valuable b) => Valuable (Mul a b) where
    evaluate (Mul lft rgt) = evaluate lft * evaluate rgt

class (Expr e) => Pretty e where
    pretty :: e -> String

instance Pretty Const where
    pretty (Const x) = show x
instance (Pretty a, Pretty b) => Pretty (Add a b) where
    pretty (Add x y) = "(" ++ pretty x ++ " + " ++ pretty y ++ ")"
instance (Pretty a, Pretty b) => Pretty (Mul a b) where
    pretty (Mul x y) = pretty x ++ " * " ++ pretty y

expr = Mul (Const 2) (Add (Const 1.5) (Const 2.5))

main = do
    putStrLn $ pretty expr
    print $ evaluate expr
