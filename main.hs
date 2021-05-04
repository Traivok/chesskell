import Moves
import Board
import Pieces
import Data.Char (ord, digitToInt)

-- Starts the game. The white pieces begin.
chesskell :: IO()
chesskell = runGame makeBoard White

-- Main game loop, run game alternating through playing colors.
runGame :: Board -> Color -> IO()
runGame board currentColor = do
  putStrLn(pretty board)
  nextMove <- getLine
  let b2 = applyToBoard board (strToMove board currentColor nextMove)
  runGame b2 (negColor currentColor)

-- Converts the input string to a Move to be applied to a Board afterwards
---- format: Pe2-e4 (pawn on e2 goes to e4)
------------ Ng1-f3 (knight on g1 goes to f3)
------------ Nf3xe5 (knight on f3 captures whatever piece is on e5) 
---- notice that the x is used when a capture happens and a - when just moving.
strToMove :: Board -> Color -> String -> Move 
strToMove b c s = Move (Piece p c origin False) destination pieceCapturing
  where
    p = parsePiece (head s)
    origin = parsePosition (s !! 1) (s !! 2)
    destination = parsePosition (s !! 4) (s !! 5)
    pieceCapturing = parseCapture (s !! 3) b ((s !! 4),(s !! 5))

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