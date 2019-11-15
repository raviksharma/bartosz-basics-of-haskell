-- Ex 4 This is an example that mimics elements of OO programming. Chess pieces are implemented as separate data types: here, for simplicity, just one, Pawn. The constructor of Pawn takes the Color of the piece and its position on the board (0-7 in both dimensions). Pieces are instances of the class Piece, which declares the following functions: color, pos, and moves. The moves function takes a piece and returns a list of possible future positions after one move (without regard to other pieces, but respecting the boundaries of the board). Define both the typeclass and the instance, so that the following program works.

data Color = White | Black
    deriving (Show, Eq)

data Pawn = Pawn Color (Int, Int)

class Piece a where
   color :: a -> Color
   pos   :: a -> (Int, Int)
   moves :: a -> [(Int, Int)]

instance Piece Pawn where
   color (Pawn c _) = c
   pos (Pawn _ pos) = pos
   moves pwn = if color pwn == White 
               then mvs (pos pwn)
               else map refl (mvs $ refl (pos pwn))
     where
       refl (x, y) = (x, 7 - y)
       mvs (x, y) = if y == 1
                    then [(x, y + 1), (x, y + 2)]
                    else if y == 7
                         then []
                         else [(x, y + 1)]
pieces = [Pawn White (3, 1), Pawn Black (4, 1), Pawn White (0, 7), Pawn Black (5, 0)]
main = do
   print $ map moves pieces
