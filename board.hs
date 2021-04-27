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
data CastlingStatus = CastlingStatus { kingSide :: Bool, queenSide :: Bool }

instance Show CastlingStatus where
    show (CastlingStatus False False) = ""
    show (CastlingStatus True False)  = "K"
    show (CastlingStatus False True)  = "Q"
    show (CastlingStatus True True)   = "KQ"
-----------------------------------------------------------------------------------------------
data Board = Board { 
        pieces        :: [Piece],
        turn          :: Color,
        fullMove      :: Int,
        halfMove      :: Int,
        enPassant     :: Maybe Square,
        whiteCastling :: CastlingStatus,
        blackCastling :: CastlingStatus
    }

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
                    Just (Piece Pawn  White _2) -> "P"
                    Just (Piece Pawn  Black _2) -> "p"
                    Just (Piece pt White _2) -> show pt
                    Just (Piece pt Black _2) -> map toLower $ show pt

instance Show Board where show = pretty

printStatus :: Board -> String
printStatus (Board _1 t fm hm ep wc bc) = intersperse ' ' $ concat [turn, full, half, pass, cas]
    where
        turn = if t == White then "w" else "b"
        full = show fm
        half = show hm
        pass = case ep of Nothing -> "-"; Just sqr -> show sqr 
        wcas = show wc 
        bcas = fmap toLower $ show bc 
        cas  = if wcas == "" && bcas == "" then "-" else wcas ++ bcas

-----------------------------------------------------------------------------------------------
emptyBoard :: Board
emptyBoard = Board [] White 0 1 Nothing cannotCastle cannotCastle
    where cannotCastle = CastlingStatus False False

makePawn :: Color -> Int -> Piece
makePawn White col = Piece Pawn White (Square col 1)
makePawn Black col = Piece Pawn Black (Square col 6)

firstRank :: Color -> Int
firstRank White = 0
firstRank Black = 7

make :: PieceType -> Color -> [Piece]
make King   c = Piece King  c (Square 4 $ firstRank c) : []
make Queen  c = Piece Queen c (Square 3 $ firstRank c) : []
make Rook   c = map (\col -> Piece Rook   c (Square col $ firstRank c)) [0, 7]
make Knight c = map (\col -> Piece Knight c (Square col $ firstRank c)) [1, 6]
make Bishop c = map (\col -> Piece Bishop c (Square col $ firstRank c)) [2, 5]
make Pawn   c = map (makePawn c) [0..7]

makePiecesByColor :: Color -> [Piece]
makePiecesByColor c = foldr (\piece acc -> (make piece c) ++ acc) [] [King .. Pawn]

makePieces :: [Piece]
makePieces = makePiecesByColor White ++ makePiecesByColor Black

makeBoard :: Board
makeBoard = Board makePieces White 0 1 Nothing cannotCastle cannotCastle 
    where cannotCastle = CastlingStatus False False
