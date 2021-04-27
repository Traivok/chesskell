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
pawnMove, knightMove, bishopMove, rookMove, queenMove, kingMove :: Piece -> Square -> Bool
---------------------------------------------------------------------------
pawnMove (Piece Pawn c from m) to = valid to && sameColumn && (advanceOne || advanceTwo)
    where
        sameColumn = sameCol from to
        advanceOne = diffRow from to == 1
        advanceTwo = not m && diffRow from to == 2
pawnMove _ _ = False
---------------------------------------------------------------------------
knightMove (Piece Knight _ from _) to = valid to && (dx, dy) `elem` [(1, 2), (2, 1)]
    where
        dx = diffRow from to
        dy = diffCol from to
knightMove _ _ = False
---------------------------------------------------------------------------
bishopMove (Piece Bishop _ from _) to = valid to && dx /= 0 && dx == dy
     where
        dx = diffRow from to
        dy = diffCol from to
bishopMove _ _ = False
---------------------------------------------------------------------------
rookMove (Piece Rook _ from _) to = valid to && ((dx == 0 && dy /= 0) || (dx /= 0 && dy == 0))
     where
        dx = diffRow from to
        dy = diffCol from to
rookMove _ _ = False
---------------------------------------------------------------------------
queenMove (Piece Queen c p m) to = rookMove (Piece Rook c p m) to || 
                                   bishopMove (Piece Bishop c p m) to
queenMove _ _ = False
---------------------------------------------------------------------------
kingMove (Piece King _ from _) to = valid to && (dx, dy) /= (0, 0) 
                                          && ((dx == 1 && dy <= 1) || (dx <= 1 && dy == 1))
    where
        dx = diffRow from to
        dy = diffCol from to
kingMove _ _ = True
---------------------------------------------------------------------------
---------------------------------------------------------------------------
data Direction = DirLeft | DirRight | Up | Down | NE | NW | SE | SW

moveGen :: Direction -> Square -> Square
moveGen DirLeft  (Square c r) = Square (c - 1) r
moveGen DirRight (Square c r) = Square (c + 1) r
--
moveGen Up   (Square c r) = Square  c (r + 1)
moveGen Down (Square c r) = Square  c (r - 1)
--
moveGen NE sq = ((moveGen Up)   . (moveGen DirRight)) sq
moveGen NW sq = ((moveGen Up)   . (moveGen DirLeft )) sq
moveGen SE sq = ((moveGen Down) . (moveGen DirRight)) sq
moveGen SW sq = ((moveGen Down) . (moveGen DirLeft )) sq

---------------------------------------------------------------------------
findDir :: Square -> Square -> Direction
findDir (Square c1 r1) (Square c2 r2) | dc == 0 && dr >  0 = Up
                                      | dc == 0 && dr <  0 = Down 
                                      | dc >  0 && dr == 0 = DirRight 
                                      | dc <  0 && dr == 0 = DirLeft
                                      | dc >  0 && dr >  0 = NE
                                      | dc <  0 && dr >  0 = NW
                                      | dc >  0 && dr <  0 = SE
                                      | dc <  0 && dr <  0 = SW
                                      | otherwise          = error "Same square"
    where 
        dc = c2 - c1
        dr = r2 - r1

---------------------------------------------------------------------------
---------------------------------------------------------------------------
block :: (Square -> Square) -> Color -> Board -> Square -> Int -> Bool
block next myColor board from n | n > 0     = (not $ valid from') && empty && block next myColor board from' (n-1)
                                | otherwise = empty || (captureAvailable square)
            where
                from'   = next from 
                square  = checkSquare board from'
                empty   = square == Nothing
                captureAvailable (Just piece) = (negColor $ color piece) == myColor 
                captureAvailable Nothing      = True 

--blockPawn   :: Color -> Board -> Square -> Int -> Bool
--blockKnight :: Color -> Board -> Square -> Int -> Bool
--blockBishop 
--blockRook
--blockQueen
--blockKing
---------------------------------------------------------------------------
-- pawnCapture :: ?
---------------------------------------------------------------------------
--moves :: Piece -> Board -> [(Move, Board)]

--check :: Board -> Maybe CheckStatus
