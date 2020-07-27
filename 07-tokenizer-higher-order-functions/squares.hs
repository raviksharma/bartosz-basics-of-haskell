-- Implement function squares that takes a list of integers and returns the list of their squares. Use higher order functions and lambdas.
squares :: [Int] -> [Int]
squares = map (\x -> x * x)

main = print $ squares [1..10]
