import qualified Data.Map as M
import Data.Char (toLower)
import Data.List (sortBy)

type Index = M.Map String Int

indexWords ::  Index -> [String] -> Index
indexWords index = 
    foldl acc index 
  where
    acc :: Index -> String -> Index
    acc ind word = 
      let n = M.findWithDefault 0 word ind in
      M.insert word (n + 1) ind

splitWords :: String -> [String]
splitWords = words . map (\c -> if elem c ".,;-\n" then ' ' else toLower c)

mostFrequent :: [String] -> [(String, Int)]
mostFrequent wrds =
    let index = indexWords M.empty wrds
    in take 9 (sortBy cmpFreq (M.toList index))
  where
    cmpFreq :: (String, Int) -> (String, Int) -> Ordering
    cmpFreq (w1, n1) (w2, n2) = compare n2 n1

main = do
    text <- readFile "moby.txt"
    print $ mostFrequent (splitWords text)
