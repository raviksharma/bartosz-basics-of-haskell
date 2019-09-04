import Data.Char

alnums :: String -> (String, String)
alnums x = als "" x

als :: String -> String -> (String, String)
als i [] = (i, [])
als i (j : rest) = if isAlphaNum j
                   then als (i ++ [j]) rest
                   else (i, j : rest)

main = print $ alnums "R2D2+C3Po"
