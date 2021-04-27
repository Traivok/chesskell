module Board where

import Pieces
import Data.List (find, intersperse)
import Data.Char (toLower)
-----------------------------------------------------------------------------------------------
data Check = Check | Mate -- | StaleMate
    deriving (Eq)

instance Show Check where
    show Check     = "+"
    show Mate      = "#"
    --show Stalemate = ""

-----------------------------------------------------------------------------------------------
data CastleStatus = CastleStatus { kingSide :: Bool, queenSide :: Bool }

instance Show CastleStatus where
    show (CastleStatus False False) = ""
    show (CastleStatus False True ) = "Q"
    show (CastleStatus True False ) = "K"
    show (CastleStatus True True  ) = "KQ"
----------------------------------------------------------------------------------------------
data Board = Board { 
        pieces        :: [Piece],
        turn          :: Color,
        fullMove      :: Int,
        halfMove      :: Int,
        enPassant     :: Maybe Square
    }
----------------------------------------------------------------------------------------------
castleStatus :: Board -> Color -> CastleStatus
castleStatus board color = CastleStatus (both && kingSide) (both && queenSide)
    where
        pred :: PieceType -> Color -> Piece -> Bool
        pred t1 c1 (Piece t2 c2 pos m) = not m  && t1 == t2 && c1 == c2
        -----
        column :: Int -> Piece -> Bool 
        column c = (== c) . (col . pos)
        -----
        rooks = filter (pred Rook color) (pieces board)
        -----
        both      = [] /= filter (pred King color) (pieces board)
        kingSide  = [] /= filter (column 7) rooks
        queenSide = [] /= filter (column 0) rooks
----------------------------------------------------------------------------------------------
checkSquare :: Board -> Square -> Maybe Piece
checkSquare board square = find (\piece -> pos piece == square) $ pieces board
----------------------------------------------------------------------------------------------
pretty :: Board -> String
pretty board = unlines $ header:(pretty' 7 $ pieces board) ++ footer
    where
        header = "  _ _ _ _ _ _ _ _"
        footer = ["  a b c d e f g h", printStatus board] 
        -----
        pretty' :: Int -> [Piece] -> [String]
        pretty' 0 pieces = [prettyRow 0 pieces]
        pretty' r pieces = prettyRow r pieces : pretty' (r - 1) pieces
        -----
        prettyRow :: Int -> [Piece] -> String
        prettyRow r pieces = (show $ r + 1) ++ '|':(prettyCol 0 r pieces)
        -----
        prettyCol :: Int -> Int -> [Piece] -> String
        prettyCol c r pieces = if valid square 
                               then sqrtStr ++ 
                                    "|"    ++ 
                                    prettyCol (c+1) r pieces 
                               else ""
            where
                square  = Square c r
                sqrtStr = case findPiece pieces square of
                    Nothing    -> "_"
                    Just piece -> let str = if pieceType piece == Pawn 
                                            then "P" 
                                            else show $ pieceType piece  
                                  in if color piece == White
                                  then str
                                  else map toLower str

instance Show Board where show = pretty

printStatus :: Board -> String
printStatus board@(Board _1 t fm hm ep) = concat $ intersperse " " [turn, full, half, pass, cas]
    where
        turn = if t == White then "w" else "b"
        full = show fm
        half = show hm
        pass = case ep of Nothing -> "-"; Just sqr -> show sqr 
        wcas = show $ castleStatus board White  
        bcas = fmap toLower $ show $ castleStatus board Black 
        cas  = if wcas == "" && bcas == "" then "-" else wcas ++ bcas
-----------------------------------------------------------------------------------------------
emptyBoard :: Board
emptyBoard = Board [] White 0 1 Nothing 

makeBoard :: Board
makeBoard = Board makePieces White 0 1 Nothing 
