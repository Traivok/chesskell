module Moves where
---------------------------------------------------------------------------
import Parser
import Pieces
import Board
import Data.Maybe (fromJust, isJust, isNothing, maybeToList)
import Data.List (find)
---------------------------------------------------------------------------
data Move = Move { piece :: Piece, square :: Square, capturing :: Maybe Piece }                              |
            Promotion { piece :: Piece, square :: Square, capturing :: Maybe Piece, promotion :: PieceType } |
            Castle { king :: Piece, rook :: Piece }
            deriving Eq

instance Show Move where
    show (Move piece square capture) = show (pieceType piece) ++ (if isNothing capture then "" else "x") ++ (show square)
    show (Promotion piece square capture promotion) = show (pieceType piece) ++ (if isNothing capture then "" else "x") ++ (show square) ++ ("=" ++ show promotion)
    show (Castle _ (Piece Rook _ (Square c _) _)) = if c == 0 then "O-O-O" else "O-O"
---------------------------------------------------------------------------
applyToBoard :: Board -> Move -> Board
applyToBoard (Board pieces turn halfMoves fullMoves enPassant) (Castle k@(Piece King kingColor kingSquare _) r@(Piece Rook rookColor rookSquare _)) = board' 
    where
        board' = Board pieces' (negColor turn) (halfMoves + 1) (if turn == Black then fullMoves + 1 else fullMoves) Nothing
        --
        mv :: Piece -> PieceType -> Piece
        mv p@(Piece tp cl (Square _ r) _) side = Piece tp cl (Square (getCol p side) r) True
        --
        kingCol, rookCol :: PieceType -> Int
        kingCol side = if side == King then 6 else 2
        rookCol side = if side == King then 5 else 3
        getCol :: Piece -> PieceType -> Int
        getCol piece = if pieceType piece == King then kingCol else rookCol
        --
        castleSide :: PieceType
        castleSide = if 0 == (col $ pos r) then Queen else King
        pieces' :: [Piece]
        pieces' = (filter (\p -> not $ p `elem` [k, r]) pieces) ++ [mv k castleSide, mv r castleSide]

applyToBoard (Board pieces turn halfMoves fullMoves enPassant) m@(Promotion piece square capturing promotion) = board'
    where
        board' = Board pieces' (negColor turn) (halfMoves') (if turn == Black then fullMoves + 1 else fullMoves) Nothing
        --
        toRemove = piece : (maybeToList capturing)
        newPiece = Piece promotion (color piece) square True
        pieces'  = newPiece : (filter (\p -> not $ p `elem` toRemove) pieces)
        --
        halfMoves' = if isCapture m || pieceType piece == Pawn then 0 else halfMoves + 1

applyToBoard (Board pieces turn halfMoves fullMoves enPassant) m@(Move piece square capturing) = board'
    where
        board' = Board pieces' (negColor turn) (halfMoves') (if turn == Black then fullMoves + 1 else fullMoves) Nothing
        --
        toRemove = piece : (maybeToList capturing)
        newPiece = Piece (pieceType piece) (color piece) square True
        pieces'  = newPiece : (filter (\p -> not $ p `elem` toRemove) pieces)
        --
        halfMoves' = if isCapture m || pieceType piece == Pawn then 0 else halfMoves + 1
---------------------------------------------------------------------------
allowedMoves :: Board -> Piece -> [Move]
allowedMoves board piece = filter isValid $ castling ++ pieceMoves board piece
    where
        rank = firstRank $ turn board
        pieceColor = color piece
        isValid :: Move -> Bool
        isValid move = let newBoard = applyToBoard board move in not $ inCheck newBoard pieceColor
        castling = castles board piece
---------------------------------------------------------------------------
allMoves :: Board -> [Move]
allMoves board = concat $ map (allowedMoves board) (filter (\p -> color p == turn board) $ pieces board)
---------------------------------------------------------------------------
threats :: Board -> Color -> [Move]
threats board attackerColor = filter captures $ concat $ map (pieceMoves board) $ filter (\p -> color p == attackerColor) $ pieces board
    where 
        captures :: Move -> Bool
        captures (Move p square (Just cap))        = color p == attackerColor
        captures (Promotion p square (Just cap) _) = color p == attackerColor
        captures _                                 = False
---------------------------------------------------------------------------
inCheck :: Board -> Color -> Bool
inCheck board kingColor = isJust $ find findMyKing $ threats board $ negColor kingColor
    where 
        findMyKing :: Move -> Bool
        findMyKing (Move      p square (Just (Piece King _ _ _))   ) = True 
        findMyKing (Promotion p square (Just (Piece King _ _ _)) _ ) = True 
        findMyKing _                                                 = False
---------------------------------------------------------------------------
inCheckMate :: Board -> Bool
inCheckMate board = (inCheck board $ turn board) && (0 == (length $ allMoves board))
---------------------------------------------------------------------------
inStaleMate :: Board -> Bool
inStaleMate board = (not $ inCheck board $ turn board) && (0 == (length $ allMoves board))
---------------------------------------------------------------------------
isDraw :: Board -> Bool
isDraw board = (inStaleMate board) || insufficientMaterial || (halfMove board >= 50)
    where
        pcs = pieces board
        whites = filter isWhite pcs
        blacks = filter isBlack pcs
        --
        count piecetype pieces = length $ filter (\p -> pieceType p == piecetype) pieces
        pieceAmounts pieces = map (\pt -> (pt, count pt pieces)) [Queen .. Pawn]
        checkMaterial pieces = case pieceAmounts pieces of 
            [(Queen, 0), (Rook, 0), (Bishop, bishops), (Knight, knights), (Pawn, 0)]  -> (bishops <= 1 && knights == 0) || (bishops == 0 && knights <= 2)
            _ -> False
        --
        insufficientMaterial = (checkMaterial whites) && (checkMaterial blacks)
---------------------------------------------------------------------------
isCapture :: Move -> Bool
isCapture (Castle _ _) = False
isCapture move        = isJust $ capturing move
---------------------------------------------------------------------------
attacked :: Board -> Color -> Square -> Bool
attacked board attackerColor = (>0) . length . (attacks board attackerColor)

attacks :: Board -> Color -> Square -> [Move]
attacks (Board pcs t hm fm ep) attackerColor target = filter captures $ concat $ map (pieceMoves board') $ filter (\p -> color p == attackerColor) $ pieces'
    where 
        pieces' = (Piece Pawn (negColor attackerColor) target True) : (filter (\p -> pos p /= target) pcs)
        board'  = Board pieces' t hm fm ep
        captures :: Move -> Bool
        captures (Move p square (Just cap))        = square == target && color p == attackerColor
        captures (Promotion p square (Just cap) _) = square == target && color p == attackerColor
        captures _                                 = False

---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- Private Functions
---------------------------------------------------------------------------
---------------------------------------------------------------------------
pieceMoves :: Board -> Piece -> [Move]
pieceMoves board pawn  @(Piece Pawn _ _ _)   = pawnMoves   board pawn
pieceMoves board knight@(Piece Knight _ _ _) = knightMoves board knight
pieceMoves board bishop@(Piece Bishop _ _ _) = bishopMoves board bishop
pieceMoves board rook  @(Piece Rook _ _ _)   = rookMoves   board rook
pieceMoves board queen @(Piece Queen _ _ _)  = queenMoves  board queen
pieceMoves board king  @(Piece King _ _ _)   = kingMoves   board king
--------------------------------------------------------------------------
-- Returns available squares and capture 
howFar :: (Square -> Square) -> Board -> Color -> Square -> [(Square, Maybe Piece)]
howFar stepper board myColor from = let to = stepper from in 
                                    if valid to == False then [] 
                                    else case findPiece (pieces board) to of
    Nothing    -> (to, Nothing) : howFar stepper board myColor to 
    Just piece -> if (color piece == myColor) then [] else [(to, Just piece)]

---------------------------------------------------------------------------
-- Knight Move
knightMoves :: Board -> Piece -> [Move]
knightMoves board knight = if pieceType knight == Knight then moves else []
    where
        moves = map fromJust $ filter (not . isNothing) $ map toMove squares
        squares :: [Square]
        squares = let Square c r = pos knight in filter valid $ map (\(dc, dr) -> Square (c + dc) (r + dr)) deltas
        deltas  = [(1, 2), (1, -2), (-1, 2), (-1, -2), (2, 1), (2, -1), (-2, 1), (-2, -1)]
        toMove :: Square -> Maybe Move
        toMove square = case findPiece (pieces board) square of 
                Nothing -> Just (Move knight square Nothing)
                Just p  -> if color p == color knight then Nothing else Just (Move knight square (Just p))
---------------------------------------------------------------------------
-- Rook Move
rookMoves :: Board -> Piece -> [Move]
rookMoves board piece = if let tp = pieceType piece in tp == Rook || tp == Queen then moves else []
    where
        movesLeft  = howFar (\(Square c r) -> Square (c - 1) r) board (color piece) (pos piece)
        movesRight = howFar (\(Square c r) -> Square (c + 1) r) board (color piece) (pos piece)
        movesUp    = howFar (\(Square c r) -> Square c (r + 1)) board (color piece) (pos piece)
        movesDown  = howFar (\(Square c r) -> Square c (r - 1)) board (color piece) (pos piece)
        moves = map (\(square, capturing) -> Move piece square capturing) $ movesLeft ++ movesRight ++ movesUp ++ movesDown
---------------------------------------------------------------------------
-- Bishop Move
bishopMoves :: Board -> Piece -> [Move]
bishopMoves board piece = if let tp = pieceType piece in tp == Bishop || tp == Queen then moves else []
    where
        movesNE = howFar (\(Square c r) -> Square (c + 1) (r + 1)) board (color piece) (pos piece)
        movesNW = howFar (\(Square c r) -> Square (c - 1) (r + 1)) board (color piece) (pos piece)
        movesSE = howFar (\(Square c r) -> Square (c + 1) (r - 1)) board (color piece) (pos piece)
        movesSW = howFar (\(Square c r) -> Square (c - 1) (r - 1)) board (color piece) (pos piece)
        moves = map (\(square, capturing) -> Move piece square capturing) $ movesNE ++ movesNW ++ movesSE ++ movesSW

---------------------------------------------------------------------------
-- Queen Move
queenMoves :: Board -> Piece -> [Move]
queenMoves board queen = if pieceType queen == Queen then moves else []
    where moves = (rookMoves board queen) ++ (bishopMoves board queen)
---------------------------------------------------------------------------
-- King Move
kingMoves :: Board -> Piece -> [Move]
kingMoves board king = if pieceType king == King then moves else []
    where
        moves = map fromJust $ filter (not . isNothing) $ map toMove squares
        squares :: [Square]
        squares = let Square c r = pos king in filter valid $ map (\(dc, dr) -> Square (c + dc) (r + dr)) deltas
        deltas  = [(1, 1), (1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1), (0, 1)]
        toMove :: Square -> Maybe Move
        toMove square = case findPiece (pieces board) square of 
                Nothing -> Just (Move king square Nothing)
                Just p  -> if color p == color king then Nothing else Just (Move king square (Just p))

castles :: Board -> Piece -> [Move]
castles board king@(Piece King c _ False) = if (inCheck board c) then [] else queenCastle ++ kingCastle
    where 
        firstRank = if c == White then 0 else 7
        anyTrue = foldr (||) False
        --
        pathBlocked, pathAttacked :: [Square] -> Bool
        pathBlocked squares  = 0 /= (length $ filter (\p -> pos p `elem` squares) $ pieces board)
        pathAttacked squares = anyTrue $ map (attacked board (negColor c)) $ squares
        findRook :: Int -> Maybe Piece
        findRook col = findPiece (pieces board) (Square col firstRank) 
        rookAv :: Int -> Bool
        rookAv col = case findRook col of
                     Nothing -> False
                     Just p  -> color p == c && pieceType p == Rook && (not $ moved p)
        --
        queenSideKingPath = [ Square 2 firstRank, Square 3 firstRank]
        queenSideRookPath = (Square 1 firstRank) : queenSideKingPath
        queenSideAv = (not $ pathBlocked queenSideRookPath) && (not $ pathAttacked queenSideKingPath) && (rookAv 0)
        queenCastle = if not queenSideAv then [] else [Castle king $ fromJust $ findRook 0]
        --
        kingSidePath = [Square 5 firstRank, Square 6 firstRank]
        kingSideAv = (not $ pathBlocked kingSidePath) && (not $ pathAttacked kingSidePath) && (rookAv 7)
        kingCastle = if not kingSideAv then [] else [Castle king $ fromJust $ findRook 7]
castles _ _ = []

---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- Pawn Moves
pawnMoves :: Board -> Piece -> [Move]
pawnMoves board piece = if pieceType piece == Pawn then moves else []
    where 
        moves :: [Move]
        moves = concat $ map toMove $ (pawnAdvance board piece) ++ (pawnCapture board piece) ++ (pawnEnPassant board piece)
        --
        toMove :: (Square, Maybe Piece) -> [Move]
        toMove p@(Square c r, _) = if (color piece == White && r == 7) || r == 0 then toPromotion p else [toMove' p]
        --
        toMove' :: (Square, Maybe Piece) -> Move
        toMove' (square, capturing) = Move piece square capturing
        --
        toPromotion :: (Square, Maybe Piece) -> [Move]
        toPromotion (square, capturing) = map (\promotion -> Promotion piece square capturing promotion) [Queen .. Knight]

pawnNext :: Color -> Square -> Square
pawnNext White (Square c r) = Square c $ r + 1
pawnNext Black (Square c r) = Square c $ r - 1

pawnCap :: Bool -> Color -> Square -> Square
pawnCap left White (Square c r) = Square (if left then c - 1 else c + 1) $ r + 1
pawnCap left Black (Square c r) = Square (if left then c - 1 else c + 1) $ r - 1

pawnAdvance :: Board -> Piece -> [(Square, Maybe Piece)]
pawnAdvance board (Piece Pawn color from moved) | emptySquare (pieces board) adv1 && (not moved) && emptySquare (pieces board) adv2 = filter (\x -> valid $ fst x) [(adv1, Nothing), (adv2, Nothing)]
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
