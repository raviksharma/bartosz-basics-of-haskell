-- Ex 5. Each card in a deck has a rank between 1 (Ace) and 13 (King) and a Suit (Club, Diamond, Heart, Spade). Write a list comprehension that generates all cards in a deck. Hint: You can encode suit as an enumeration deriving Show and Enum. The Enum type class will let you create ranges like [Club .. Spade] (put space before .. or the parser will be confused).

data Suit = Club | Diamond | Heart | Spade
    deriving (Show, Enum)

data Rank = Rank Int

instance Show Rank where
    show (Rank 1)  = "Ace"
    show (Rank 11) = "Jack"
    show (Rank 12) = "Queen"
    show (Rank 13) = "King"
    show (Rank i)  = show i

deck = [(Rank r, s) | s <- [Club .. Spade]
                    , r <- [1..13]]

main = print deck
