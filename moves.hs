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
--move mv = case pieceType $ piece mv of
    --Pawn   -> canPawnMove (piece mv) to 
    --Knight -> knightMove from to 
    --Bishop -> bishopMove from to 
    --Rook   -> rookMove   from to 
    --Queen  -> (rookMove  from to) || (bishopMove from to) 
    --King   -> kingMove   from to 
    --where
        --to = dest mv
---------------------------------------------------------------------------
pawnMoved :: Piece -> Bool
pawnMoved (Piece Pawn c p) = let r = row p in
                                if c == White 
                                then r /= 1 
                                else r /= 6
pawnMoved (Piece _1 _2 _3) = False

canPawnMove :: Piece -> Square -> Bool
canPawnMove pawn@(Piece Pawn c from) to = validTo && sameColumn && (advanceOne || advanceTwo)
    where
        validTo    = valid to
        sameColumn = sameCol from to
        advanceOne = diffRow from to == 1
        advanceTwo = (not $ pawnMoved pawn) && diffRow from to == 2
canPawnMove _1 _2 = False



--moves :: Piece -> Board -> [(Move, Board)]

--check :: Board -> Maybe CheckStatus
