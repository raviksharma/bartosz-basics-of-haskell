-- Implement function translate, which takes a dictionary and a list of strings and returns a list of translated strigs. If a string is not in a dictionary, it should be replaced with "whatchamacallit". For bonus points, try using the higher order map function from the Prelude, and the where clause. Remember that a function defined inside where has access to the arguments of the outer function.

import qualified Data.Map as M

type Dict = M.Map String String

translate :: Dict -> [String] -> [String]
translate dict words = map trans words
  where
    trans :: String -> String
    trans w =
      case M.lookup w dict of
        (Just w') -> w'
        Nothing   -> "whatchamacallit"

testTranslation :: Dict -> IO ()
testTranslation dict = do
    print $ translate dict ["where", "is", "the", "colosseum"]

testInsertion :: Dict -> IO Dict
testInsertion dict = do
    return $ M.insert "colosseum" "colosseo" dict
    
main = 
    let dict = M.fromList [("where", "dove"), ("is", "e"), ("the", "il")]
    in do
          testTranslation dict
          dict'  <- testInsertion dict
          testTranslation dict'
          putStrLn "The original dictionary is unchanged:"
          testTranslation dict
