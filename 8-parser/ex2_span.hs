-- Write a parser that splits a string into a list of words using space characters as separators (use function isSpace).

import Data.Char
import Prelude hiding (Word)

type Word = String

sentence :: String -> [Word]
sentence "" = []
sentence str = let (w, str') = word str
               in w : sentence str'

-- returns a word and the rest of input
word :: String -> (Word, String)
word str = let (w, str')  = span (not . isSpace) str
               (_, str'') = span isSpace str'
           in (w, str'')
 
main = print $ sentence "Ceci n'est pas une phrase"
