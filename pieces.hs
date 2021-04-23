module Pieces where

import Data.Char (ord, chr, toLower)
import Data.Maybe(fromJust)
import Parser

class Evaluate a where eval :: a -> Double
class Validate a where valid :: a -> Bool

--------------------------------------------------------------------------- 
--------------------------------------------------------------------------- 
data Position = Position { col :: Char, row :: Int }
    deriving Eq

instance Show Position where
    show (Position col row) = col : show row

instance Validate Position where
    valid (Position col row) = ('a' <= col && col <= 'h') && (1 <= row && row <= 8)

readPos :: String -> Maybe Position
readPos str = (\(col, row) -> Position col (ord row - ord '0')) <$> mparsed
    where
    isCol c = ('a' <= c && c <= 'h')
    isRow r = ('1' <= r && r <= '8') 
    mparsed = maybeParse (pure (,) <*> (satisfy isCol) <*> (satisfy isRow) <* nil) str
--strToPos (col:row:[]) = Position col (read row)
--strToPos _            = Nothing
--------------------------------------------------------------------------- 
--------------------------------------------------------------------------- 
data PieceType = King | Queen | Rook | Bishop | Knight | Pawn
    deriving (Eq, Enum)

-- WARNING dont evaluate king, this could cause overflow or innacuracy results
-- instead evaluate checkmate as infinity and stop the calculation
instance Evaluate PieceType where
    eval = fromJust . (flip lookup [(King, 10000), (Queen, 9), (Rook, 5), (Bishop, 3), (Knight, 2.8), (Pawn, 1)])

instance Show PieceType where
    show = fromJust . (flip lookup [(King, "K"), (Queen, "Q"), (Rook, "R"), (Bishop, "B"), (Knight, "N"), (Pawn, "")])
-------------------------------------------------------------------------- 
-- True for White, False for Black
data Color = W | B
    deriving (Eq, Enum)
--------------------------------------------------------------------------- 
data Piece = Piece { pieceType :: PieceType, color :: Color, pos :: Position }
    deriving Eq

instance Evaluate Piece where
    eval piece = if white then value else -value 
        where
        white = W == color piece 
        value = eval $ pieceType piece

-- be careful, moves can have ambiguity, capture, check and mates, instantiate for move instead
instance Show Piece where
    show piece | pieceType piece == Pawn = showPawn piece
               | otherwise               = showPiece piece
        where
        showPiece piece = let str = show $ pieceType piece in if W == color piece then str else lower str
        showPawn piece = show $ col $ pos piece
        lower = map toLower
