import Moves
import Board
import Pieces
import Data.Char (ord, digitToInt)

-- Starts the game. The white pieces begin.
chesskell :: IO()
chesskell = runGame makeCastleAvailable 

-- Main game loop, run game alternating through playing colors.
runGame :: Board -> IO()
runGame board | isDraw board      = putStrLn(pretty board) >> putStrLn "Draw" 
              | inCheckMate board = putStrLn(pretty board) >> putStrLn "Mate"
              | otherwise = do 
  putStrLn(pretty board)
  nextMove <- getLine
  let mv = strToMove board nextMove in 
    if validMove mv board
    then runGame (applyToBoard board mv) 
    else error "Invalid Move"

-- Converts the input string to a Move to be applied to a Board afterwards
---- format: Pe2-e4 (pawn on e2 goes to e4)
------------ Ng1-f3 (knight on g1 goes to f3)
------------ Nf3xe5 (knight on f3 captures whatever piece is on e5) 
---- notice that the x is used when a capture happens and a - when just moving.
strToMove :: Board -> String -> Move 
strToMove b "O-O-O" = strToCastle b 0
strToMove b "O-O"   = strToCastle b 7

strToMove b s = Move piece destination pieceCapturing
  where
    c = turn b
    p = parsePiece (head s)
    origin = parsePosition (s !! 1) (s !! 2)
    destination = parsePosition (s !! 4) (s !! 5)
    pieceCapturing = parseCapture (s !! 3) b ((s !! 4),(s !! 5))
    piece = case checkSquare b origin of
                    Nothing -> error "There isn't any piece in that square"
                    Just pc -> if color pc == c && pieceType pc == p then pc else error "Invalid Piece"

-- Check if Move is valid
validMove :: Move -> Board -> Bool
validMove move board = move `elem` (allMoves board)

-- Fetch Castle
strToCastle :: Board -> Int -> Move
strToCastle b rookCol = Castle king rook
    where
        rank = firstRank $ turn b
        king = case checkSquare b (Square 4 rank) of
            Nothing -> error "Invalid castle"
            Just k  -> k
        rook = case checkSquare b (Square rookCol rank) of
            Nothing -> error "Invalid castle"
            Just r  -> r
 
-- Given a char, return a piecetype.
parsePiece :: Char -> PieceType
parsePiece c
  | c == 'P' = Pawn
  | c == 'K' = King 
  | c == 'Q' = Queen 
  | c == 'R' = Rook 
  | c == 'B' = Bishop 
  | otherwise = Knight

-- parse 2 chars into a square (board coordinate)
parsePosition :: Char -> Char -> Square 
parsePosition col row = Square (ord col - 97) (digitToInt row - 1)

-- parse a capture. if it its a - (no capture) ignore it. Otherwise (x) check
  -- the square where the capture is intended to return the piece that was there.
parseCapture :: Char -> Board -> (Char, Char) -> Maybe Piece 
parseCapture cap board pos
  | cap == '-' = Nothing
  | otherwise = checkSquare board (uncurry parsePosition pos)
