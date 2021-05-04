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
        halfMove      :: Int,
        fullMove      :: Int,
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
pretty board = unlines $ header:(pretty' 7 $ pieces board) ++ footer ++ [(show (turn board)) ++ "'s Turn: "]
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
                    Just (Piece tp c _ _) -> if c == White then show tp else map toLower $ show tp
-----------------------------------------------------------------------------------------------
fen :: Board -> String
fen board = (absFen 7 $ pieces board) ++ " " ++ printStatus board 
    where
        absFen :: Int -> [Piece] -> String
        absFen 0 p = fenRow 0 p 
        absFen n p = fenRow n p ++ '/':(absFen (n - 1) p)
        -----
        fenRow :: Int -> [Piece] -> String
        fenRow n pieces = fenCol 0 n pieces 0
         -----
        fenCol :: Int -> Int -> [Piece] -> Int -> String
        fenCol c r pieces last = if valid square 
                                 then curr ++ next
                                 else lastStr
            where
                square = Square c r
                lastStr = if last == 0 then "" else show last
                curr = case findPiece pieces square of
                    Nothing    -> ""
                    Just piece -> lastStr ++ (show $ pieceType piece)
                next = fenCol (c + 1) r pieces (if findPiece pieces square == Nothing then last + 1 else 0) 
            
instance Show Board where show = pretty

printStatus :: Board -> String
printStatus board = concat $ intersperse " " [wOrB, cas, pass, half, full]
    where
        wOrB = if turn board == White then "w" else "b"
        full = show $ fullMove board
        half = show $ halfMove board
        pass = case enPassant board of Nothing -> "-"; Just sqr -> show sqr 
        wcas = show $ castleStatus board White  
        bcas = fmap toLower $ show $ castleStatus board Black 
        cas  = if wcas == "" && bcas == "" then "-" else wcas ++ bcas
-----------------------------------------------------------------------------------------------
-- Constant Boards
-----------------------------------------------------------------------------------------------
emptyBoard :: Board
emptyBoard = Board [] White 0 1 Nothing 

makeBoard :: Board
makeBoard = Board makePieces White 0 1 Nothing 

makeKingsPawn = Board pcs Black 0 1 $ Just $ Square 4 2 
    where 
        pcs = map f makePieces
        f p@(Piece tp c sq m) = if sq == (Square 4 1) then (Piece tp c (Square 4 3) True) else p
-----------------------------------------------------------------------------------------------
makeEnPassAvailable = Board pcs White 0 3 $ Just $ Square 5 5 
    where
        pcs = news ++ olds
        news = [(Piece Pawn White (Square 4 4) True), (Piece Pawn Black (Square 3 5) True), (Piece Pawn Black (Square 5 4) True)]
        olds :: [Piece]
        olds = filter f makePieces
        f :: Piece -> Bool
        f p = not $ pos p `elem` [(Square 4 1), (Square 3 6), (Square 5 6)]
-----------------------------------------------------------------------------------------------
makeCheck = Board [wKing, wQueen, bKing] Black 5 60 Nothing
    where
        wKing  = Piece King  White (Square 7 5) True 
        wQueen = Piece Queen White (Square 5 5) True 
        bKing  = Piece King  Black (Square 7 7) True 

makeCheckMate = Board [wKing, wQueen, bKing] Black 5 60 Nothing
    where
        wKing  = Piece King  White (Square 7 5) True 
        wQueen = Piece Queen White (Square 6 6) True 
        bKing  = Piece King  Black (Square 7 7) True 

makeStaleMate = Board [wKing, wQueen, bKing] Black 5 60 Nothing
    where
        wKing  = Piece King  White (Square 7 5) True 
        wQueen = Piece Queen White (Square 5 6) True 
        bKing  = Piece King  Black (Square 7 7) True 
-----------------------------------------------------------------------------------------------
makeCastleAttack = Board pcs White 5 60 Nothing        
    where
        pcs = wKing ++ wRooks ++ bKing ++ bRooks ++ attackers
        wKing  = make King White
        wRooks = make Rook White
        bKing  = make King Black
        bRooks = make Rook Black
        attackers = map (\c -> Piece Rook Black (Square c 2) True) [2, 3, 5, 6]

makeCastleAvailable = Board pcs White 5 60 Nothing
    where
        pcs = wPawns ++ bPawns ++ (pieces makeCastleAttack)
        wPawns = make Pawn White
        bPawns = make Pawn Black

makeCastleBlock = Board pcs White 5 60 Nothing        
    where
        pcs = (make King Black) ++ (make King White) ++ (make Rook Black) ++ (make Rook White) ++ [Piece Knight Black (Square 1 0) True] ++ (make Knight Black)
    
makeCastleCheck = Board pcs White 5 60 Nothing        
    where
        pcs = (make King Black) ++ (make King White) ++ (make Rook Black) ++ (make Rook White) ++ [Piece Queen Black (Square 4 3) True, Piece Queen White (Square 4 4) True]
 
