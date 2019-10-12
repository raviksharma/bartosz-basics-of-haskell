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
word "" = ("", "")
word (c:cs) | isSpace c = ("", cs)
            | otherwise = let (w, cs') = word cs
                          in (c:w, cs')
                           
main = print $ sentence "Ceci n'est pas une phrase"
