-- Implement the function toInts from the previous tutorial using map. This function takes a string of digits and creates a list of Ints corresponding to these digits.
import Data.Char

toInts :: String -> [Int]
toInts = map digitToInt

main = print $ toInts "30750"
