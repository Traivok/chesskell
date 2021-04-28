import Test.QuickCheck
import Pieces
import Board
import Moves

movedPawnAdvance = map (pawnAdvance emptyBoard) $ (makePawns White 2) ++ (makePawns Black 5)
    where 
        makePawns color row = map (\c -> Piece Pawn color (Square c row) True) [0..7]

pawnAdvances = map (pawnAdvance makeBoard) pawns
    where 
       pawns = filter (\p -> pieceType p == Pawn) $ pieces makeBoard 

pawnBlock = map (pawnAdvance makeBoard) pawns
    where 
        makePawns color row moved = map (\c -> Piece Pawn color (Square c row) moved) [0..7]
        w6p = makePawns White 5 False
        w5p = makePawns White 4 False
        b4p = makePawns Black 3 False
        b3p = makePawns Black 2 False
        pawns = w6p ++ w5p ++ b4p ++ b3p

pawnCaptures = map (pawnCapture makeBoard) pawns
    where 
        makePawns color row = map (\c -> Piece Pawn color (Square c row) True) [0..7]
        w7p = makePawns White 6
        w6p = makePawns White 5 
        w5p = makePawns White 4
        b4p = makePawns Black 3 
        b3p = makePawns Black 2
        b2p = makePawns Black 1
        pawns = w7p ++ w6p ++ w5p ++ b4p ++ b3p ++ b2p

makeEnPass :: Color -> Int -> Board
makeEnPass color col = Board [pawn] White 0 1 (Just enPass)
    where
        enPass = Square col (if color == White then 2 else 5)
        pieceSquare = Square col (if color == White then 3 else 4)
        pawn = Piece Pawn color pieceSquare True

enPass color col = map (pawnEnPassant (makeEnPass color col)) pawns
    where
        makePawns color row moved = map (\c -> Piece Pawn color (Square c row) moved) [0..7]
        w6p = makePawns White 5 False
        w5p = makePawns White 4 False
        b4p = makePawns Black 3 False
        b3p = makePawns Black 2 False
        pawns = w6p ++ w5p ++ b4p ++ b3p

testPawnMoves = map (pawnMoves makeBoard) pawns
    where 
       pawns = filter (\p -> pieceType p == Pawn) $ pieces makeBoard 

