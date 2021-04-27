module Board where

import Pieces
import Data.List (find)
import Data.Char (toLower)
-----------------------------------------------------------------------------------------------
data Check = Check | Mate -- | StaleMate
    deriving (Eq)

instance Show Check where
    show Check     = "+"
    show Mate      = "#"
    --show Stalemate = ""
-----------------------------------------------------------------------------------------------
data Sides = QueenSide | KingSide
-----------------------------------------------------------------------------------------------
data CastlingStatus = CastlingStatus { queenSide :: Bool, kingSide :: Bool }
-----------------------------------------------------------------------------------------------
data Board = Board { 
        pieces :: [Piece],
        enPassant :: Maybe Square,
        whiteCastling :: CastlingStatus,
        blackCastling :: CastlingStatus,
        check :: Maybe Check
    }

pretty :: Board -> String
pretty board = unlines $ "  _ _ _ _ _ _ _ _":(pretty' 7 $ pieces board) ++ ["  a b c d e f g h"]
    where
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

-----------------------------------------------------------------------------------------------
emptyBoard :: Board
emptyBoard = Board [] Nothing cannotCastle cannotCastle Nothing
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
makeBoard = Board makePieces Nothing cannotCastle cannotCastle Nothing
    where cannotCastle = CastlingStatus False False
