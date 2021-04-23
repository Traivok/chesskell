module Board where

import Pieces
-----------------------------------------------------------------------------------------------
data Check = Check | Mate
    deriving (Eq)

instance Show Check where
    show Check = "+"
    show Mate  = "#"
-----------------------------------------------------------------------------------------------
data EnPassant = Maybe Position
-----------------------------------------------------------------------------------------------
data Move = Move { piece :: Piece, target :: Position }
-----------------------------------------------------------------------------------------------
data GameMove = GameMove { move :: Move, capture :: Bool }
-----------------------------------------------------------------------------------------------
data Board = Board { 
        pieces :: [Piece],
        enPassant :: Maybe Position
    }
