module Pieces where

import Data.Char (ord, chr, toLower)
import Data.Maybe(fromJust)
import Parser

class Evaluate a where eval :: a -> Double
class Validate a where valid :: a -> Bool

--------------------------------------------------------------------------- 
--------------------------------------------------------------------------- 
data Position = Position { col :: Int, row :: Int }
    deriving Eq

instance Show Position where
    show (Position c r) = col : show (r + 1)
        where col = (chr (c + ord 'a')) 

instance Validate Position where
    valid (Position col row) = (inRange col) && (inRange row)
        where inRange p = 0 <= p && p <= 7

readPos :: String -> Maybe Position
readPos str = (\(col, row) -> Position ((ord col - ord 'a')) (ord row - ord '0')) <$> mparsed
    where
    isCol c = ('a' <= c && c <= 'h')
    isRow r = ('1' <= r && r <= '8') 
    mparsed = maybeParse (pure (,) <*> (satisfy isCol) <*> (satisfy isRow) <* nil) str
--------------------------------------------------------------------------- 
--------------------------------------------------------------------------- 
data PieceType = King | Queen | Rook | Bishop | Knight | Pawn
    deriving (Eq, Enum)

instance Evaluate PieceType where
    eval = fromJust . (flip lookup [(King, 0), (Queen, 9), (Rook, 5), (Bishop, 3), (Knight, 2.8), (Pawn, 1)])

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
