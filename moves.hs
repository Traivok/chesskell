module Moves where
---------------------------------------------------------------------------
import Parser
import Pieces
import Board
---------------------------------------------------------------------------
data Move = Move { 
        piece     :: Piece, 
        dest      :: Square, 
        captures  :: Bool,
        promotion :: Maybe PieceType
    }
---------------------------------------------------------------------------
--move :: Move -> Bool
--move mv = case pieceType pc of
    --Pawn   -> pawnMove pc to 
    --Knight -> knightMove pc to 
    --Bishop -> bishopMove pc to 
    --Rook   -> rookMove   pc to 
    --Queen  -> (rookMove  pc to) || (bishopMove pc to) 
    --King   -> kingMove   pc to 
    --where
        --pc = piece mv
        --to = dest mv
---------------------------------------------------------------------------
---------------------------------------------------------------------------
pawnDelta, knightDelta, bishopDelta, rookDelta, queenDelta, kingDelta :: Piece -> Square -> Bool
---------------------------------------------------------------------------
pawnDelta (Piece Pawn c from m) to = sameColumn && (advanceOne || advanceTwo)
    where
        sameColumn = sameCol from to
        advanceOne = diffRow from to == 1
        advanceTwo = not m && diffRow from to == 2
pawnDelta _ _ = False
---------------------------------------------------------------------------
knightDelta (Piece Knight _ from _) to = (dx, dy) `elem` [(1, 2), (2, 1)]
    where
        dx = diffRow from to
        dy = diffCol from to
knightDelta _ _ = False
---------------------------------------------------------------------------
bishopDelta (Piece Bishop _ from _) to = dx /= 0 && dx == dy
     where
        dx = diffRow from to
        dy = diffCol from to
bishopDelta _ _ = False
---------------------------------------------------------------------------
rookDelta (Piece Rook _ from _) to = (dx == 0 && dy /= 0) || (dx /= 0 && dy == 0)
     where
        dx = diffRow from to
        dy = diffCol from to
rookDelta _ _ = False
---------------------------------------------------------------------------
queenDelta (Piece Queen c p m) to = rookDelta (Piece Rook c p m) to || 
                                    bishopDelta (Piece Bishop c p m) to
queenDelta _ _ = False
---------------------------------------------------------------------------
kingDelta (Piece King _ from _) to = (dx == 1 && dy <= 1) || (dx <= 1 && dy == 1)
    where
        dx = diffRow from to
        dy = diffCol from to
kingDelta _ _ = True
---------------------------------------------------------------------------
checkDeltasAndBound :: Piece -> Square -> Bool
checkDeltasAndBound piece to = valid to &&  case pieceType piece of 
    Pawn   -> pawnDelta   piece to
    Knight -> knightDelta piece to 
    Bishop -> bishopDelta piece to 
    Rook   -> rookDelta   piece to
    Queen  -> queenDelta  piece to 
    King   -> kingDelta   piece to
---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- Returns available squares and capture 
howFar :: (Square -> Square) -> Board -> Color -> Square -> ([Square], Maybe Piece) 
howFar stepper board myColor from = let to = stepper from in case findPiece (pieces board) to of
    Nothing    -> let res = howFar stepper board myColor to in (to : (fst res), snd res)
    Just piece -> if (color piece == myColor) then ([], Nothing) else ([to], Just piece)

---------------------------------------------------------------------------
---------------------------------------------------------------------------
pawnMoves :: Board -> Piece -> [(Square, Maybe Piece)]
pawnMoves board piece = if pieceType piece == Pawn then moves else []
    where moves = (pawnAdvance board piece) ++ (pawnCapture board piece) ++ (pawnEnPassant board piece)

pawnNext :: Color -> Square -> Square
pawnNext White (Square c r) = Square c $ r + 1
pawnNext Black (Square c r) = Square c $ r - 1

pawnCap :: Bool -> Color -> Square -> Square
pawnCap left White (Square c r) = Square (if left then c - 1 else c + 1) $ r + 1
pawnCap left Black (Square c r) = Square (if left then c - 1 else c + 1) $ r - 1

pawnAdvance :: Board -> Piece -> [(Square, Maybe Piece)]
pawnAdvance board (Piece Pawn color from moved) | emptySquare (pieces board) adv1 && (not moved) && emptySquare (pieces board) adv2 = [(adv1, Nothing), (adv2, Nothing)]
                                                | emptySquare (pieces board) adv1 = [(adv1, Nothing)]
                                                | otherwise                   = []
    where 
        adv1 = pawnNext color from
        adv2 = pawnNext color adv1 
pawnAdvance _ _ = []

pawnCapture :: Board -> Piece -> [(Square, Maybe Piece)]
pawnCapture board (Piece Pawn c from _) = filter check [(left, findPiece enemies left), (right, findPiece enemies right)]
    where
        enemies = filter (\p -> color p /= c) $ pieces board
        left  = pawnCap True  c from
        right = pawnCap False c from
        check :: (Square, Maybe Piece) -> Bool
        check (_, Nothing) = False
        check (square, _)  = valid square 
pawnCapture _ _ = []

enPassToPos :: Square -> Square
enPassToPos (Square c 2) = Square c 3
enPassToPos (Square c 5) = Square c 4 
enPassToPos square       = error $ "Invalid En Passant Square " ++ show square

pawnEnPassant :: Board -> Piece -> [(Square, Maybe Piece)]
pawnEnPassant board (Piece Pawn c from _) = if fifthRank then move else []
    where 
        fifthRank = let r = row from in (r == 4 && c == White) || (r == 3 && c == Black)
        adjacency p = 1 == diffCol from (pos p) 
        enPass = case enPassant board of
            Nothing     -> []
            Just square -> [(square, findPiece (pieces board) (enPassToPos square))] 
        move = case enPass of
            []                  -> []
            [(square, Nothing)] -> error $ "Invalid En Passant " ++ show square
            [(square, Just p)]  -> if color p /= c && adjacency p then [(square, Just p)] else []
pawnEnPassant _ _ = []

