--Implement a function that takes a pair of lists and returns a list of pairs. For instance  ([1, 2, 3, 4], [1, 4, 9]) should produce [(1, 1), (2, 4), (3, 9)]. Notice that the longer of the two lists is truncated if necessary. Use nested patterns.
zipLst :: ([a], [b]) -> [(a, b)]
zipLst ((x : xs), (y: ys)) = (x, y) : zipLst (xs, ys)
zipLst (_, _) = []

main = print $ zipLst ([1, 2, 3, 4], "Hello")
