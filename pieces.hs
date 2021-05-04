--------------------------------------------------------------------------- 
module Pieces where
--------------------------------------------------------------------------- 
import Data.Char (ord, chr, toLower)
import Data.Maybe (isNothing, fromJust)
import Data.List (find)
import Parser
--------------------------------------------------------------------------- 
class Validate a where valid :: a -> Bool
--------------------------------------------------------------------------- 
data Square = Square { col :: Int, row :: Int }
    deriving Eq

instance Show Square where
    show (Square c r) = col : show (r + 1)
        where col = (chr (c + ord 'a')) 

squareToTuple :: Square -> (Int, Int)
squareToTuple (Square c r) = (c, r)

tupleToSquare :: (Int, Int) -> Square
tupleToSquare (c, r) = Square c r

instance Validate Square where
    valid (Square col row) = (inRange col) && (inRange row)
        where inRange p = 0 <= p && p <= 7

sameRow :: Square -> Square -> Bool
sameRow (Square _1 c1) (Square _2 c2) = c1 == c2

sameCol :: Square -> Square -> Bool
sameCol (Square r1 _1) (Square r2 _2) = r1 == r2

diffRow :: Square -> Square -> Int
diffRow (Square _1 c1) (Square _2 c2) = abs $ c1 - c2

diffCol :: Square -> Square -> Int
diffCol (Square r1 _1) (Square r2 _2) = abs $ r1 - r2

diff :: Square -> Square -> Square
diff s1 s2 = let (dc, dr) = (diffCol s1 s2, diffRow s1 s2) in Square dc dr

readSquare :: String -> Maybe Square
readSquare str = (\(col, row) -> Square ((ord col - ord 'a')) (ord row - ord '0')) <$> mparsed
    where
    isCol c = ('a' <= c && c <= 'h')
    isRow r = ('1' <= r && r <= '8') 
    mparsed = maybeParse (pure (,) <*> (satisfy isCol) <*> (satisfy isRow) <* nil) str
--------------------------------------------------------------------------- 
data PieceType = King | Queen | Rook | Bishop | Knight | Pawn
    deriving (Eq, Enum)

instance Show PieceType where
    show = fromJust . (flip lookup [(King, "K"), (Queen, "Q"), (Rook, "R"), (Bishop, "B"), (Knight, "N"), (Pawn, "P")])
-------------------------------------------------------------------------- 
data Color = White | Black
    deriving (Eq, Enum, Show)

negColor :: Color -> Color
negColor White = Black
negColor Black = White
-------------------------------------------------------------------------- 
data Piece = Piece { pieceType :: PieceType, color :: Color, pos :: Square, moved :: Bool }
    deriving Eq

-- Debug only, show move for alg. notation
instance Show Piece where 
    show piece = (let str = show $ pieceType piece in if isWhite piece then str else map toLower str) ++ (show $ pos piece)

findPiece :: [Piece] -> Square -> Maybe Piece
findPiece pieces square = find sameSquare pieces
    where sameSquare piece = pos piece == square

emptySquare :: [Piece] -> Square -> Bool
emptySquare = (isNothing .) . findPiece 

isBlack, isWhite :: Piece -> Bool
isWhite p = color p == White
isBlack = not . isWhite
-------------------------------------------------------------------------- 
makePawn :: Color -> Int -> Piece
makePawn White col = Piece Pawn White (Square col 1) False
makePawn Black col = Piece Pawn Black (Square col 6) False

firstRank :: Color -> Int
firstRank White = 0
firstRank Black = 7

make :: PieceType -> Color -> [Piece]
make King   c = Piece King  c (Square 4 $ firstRank c) False : []
make Queen  c = Piece Queen c (Square 3 $ firstRank c) False : []
make Rook   c = map (\col -> Piece Rook   c (Square col $ firstRank c) False) [0, 7]
make Knight c = map (\col -> Piece Knight c (Square col $ firstRank c) False) [1, 6]
make Bishop c = map (\col -> Piece Bishop c (Square col $ firstRank c) False) [2, 5]
make Pawn   c = map (makePawn c) [0..7]

makePiecesByColor :: Color -> [Piece]
makePiecesByColor c = foldr (\piece acc -> (make piece c) ++ acc) [] [King .. Pawn]

makePieces :: [Piece]
makePieces = makePiecesByColor White ++ makePiecesByColor Black
