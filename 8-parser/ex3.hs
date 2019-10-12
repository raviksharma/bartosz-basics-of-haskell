-- Generalize the sentence parser from (Ex 2) to take a pluggable parser. The new function is called several and takes as an argument a generic function String->(a, String), which is supposed to parse a string and return the result of type a together with the leftover string. Use it to split a string into a list of numbers.

import Data.Char

type Parser a = String -> (a, String)

several :: Parser a -> String -> [a]
several p "" = []
several p str = let (a, str') = p str
                    as = several p str'
                in a:as

num :: Parser Int
num str = 
    let (digs, str') = span isDigit str
        (_, str'')   = span isSpace str'
    in (read digs, str'')


word :: Parser String
word str = let (w, str')  = span (not . isSpace) str
               (_, str'') = span isSpace str'
           in (w, str'')

main = do
    print $ several num "12 4 128"
    print $ several word "Ceci n'est pas une phrase"
