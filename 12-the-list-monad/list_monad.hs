import Control.Applicative
import Control.Monad (liftM, ap)

data List a = Nil | Cons a (List a)

instance (Show a) => Show (List a) where
    show Nil = ""
    show (Cons x xs) = show x ++ ", " ++ show xs

instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure  = return
  (<*>) = ap

instance Monad List where
    return x = Cons x Nil
    xs >>= k = join $ fmap k xs

join :: List (List a) -> List a
join Nil = Nil
join (Cons xs xss) =  cat xs (join xss)

cat :: List a -> List a -> List a
cat Nil ys = ys
cat (Cons x xs) ys = Cons x (cat xs ys)

neighbors :: (Num a) => a -> a -> List a
neighbors x dx = Cons (x - dx) (Cons x (Cons (x + dx) Nil))


test = do
    x <- neighbors 0 100
    y <- neighbors x 1
    return y
    
main = print $ test
