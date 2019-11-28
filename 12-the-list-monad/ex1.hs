-- Ex 1. Define join for Maybe (don't be surprised how simple it is):

join :: Maybe (Maybe a) -> Maybe a
join Nothing = Nothing
join (Just mb) = mb

test1, test2, test3 :: Maybe (Maybe String)
test1 = Nothing
test2 = Just Nothing
test3 = Just (Just "a little something")

main = do
    print $ join test1
    print $ join test2
    print $ join test3
