module Board where

import Pieces
-----------------------------------------------------------------------------------------------
data Check = Check | Mate
    deriving (Eq)

instance Show Check where
    show Check = "+"
    show Mate  = "#"

-----------------------------------------------------------------------------------------------
data Move = Move { piece :: Piece, target :: Position } | 
            --Castling { color :: Color, side :: Bool }
            --EnPassant?
            --Promotion?
            --Captures?
instance Show Move where 
    show = ""
-----------------------------------------------------------------------------------------------
data Board = Board { 
        pieces :: [Piece],
        --enPassantStatus :: Maybe EnPassant,
        --whiteCastling :: CastlingStatus,
        --blackCastling :: CastlingStatus,
        check :: Maybe Check
    }
