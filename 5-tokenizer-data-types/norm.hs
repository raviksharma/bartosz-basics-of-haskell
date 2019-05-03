--Implement norm that takes a list of Doubles and returns the square root (sqrt) of the sum of squares of its elements.

norm :: [Double] -> Double
norm lst = sqrt $ doublessum lst

doublessum :: [Double] -> Double
doublessum (i : rest) = i * i + doublessum rest
doublessum [] = 0.0

main = do
   print (norm [1.1, 2.2, 3.3])
   print (norm [3.0, 4.0])
