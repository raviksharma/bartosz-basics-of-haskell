import Data.Char
import Prelude hiding (span)
-- show
span :: (a -> Bool) -> [a] -> ([a], [a])
span pred str = 
  let -- define a helper function 'spanAcc'
    spanAcc acc [] = (acc, [])
    spanAcc acc (c : cs) | pred c = 
                             let (acc', cs') = spanAcc acc cs 
                             in (c:acc', cs')
                         | otherwise = (acc, c:cs)
  in
    spanAcc [] str

main = do
    print $ span isAlphaNum "R2D2 + C3Po"
    print $ span isDigit "Y22D + C3Po"
    print $ span isDigit "22D + C3Po"
