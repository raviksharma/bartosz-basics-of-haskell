import Data.Char

alnums :: String -> (String, String)
alnums str = als "" str
  where
    als acc [] = (acc, [])
    als acc (c : cs) | isAlphaNum c = 
                           let (acc', cs') = als acc cs 
                           in (c:acc', cs')
                     | otherwise = (acc, c:cs)

main = print $ alnums "R2D2+C3Po"
