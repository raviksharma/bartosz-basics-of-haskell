-- Just as a proof of concept, implement a version of alnums using foldl
import Data.Char

type Accum = (Bool, String, String)

alnums :: String -> (String, String)
alnums str = let (_, als, rest) = foldl f (True, [], []) str
             in (als, rest)
  where
    f (True, als, rest) c  | isAlphaNum c = (True, als ++ [c], rest)
                           | otherwise = (False, als, [c])
    f (False, als, rest) c = (False, als, rest ++ [c])

main = do
    print $ alnums "R2D2+C3Po"
    print $ alnums "a14"
