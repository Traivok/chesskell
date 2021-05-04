import Moves
import Board
import Pieces

sg :: IO()
sg = runGame makeBoard

runGame :: Board -> IO()
runGame board = do
  putStrLn(pretty board)
  nextMove <- getLine
  let b2 = applyToBoard board (strToMove nextMove)
  runGame b2

strToMove :: String -> Move 
strToMove s = Move (Piece p c (Square 1 1) False) (Square 1 2) Nothing
  where
    p = parsePiece (head s)
    c = parseColor (head s)
    origin = parseOrigin (s !! 1) (s !! 2)
    destination = parseDestination (s !! 4) (s !! 5)
    pieceCapturing = parseCapture (s !! 3)

parsePiece :: Char -> PieceType
parsePiece c
  | c == 'P' = Pawn
  | c == 'K' = King 
  | c == 'Q' = Queen 
  | c == 'R' = Rook 
  | c == 'B' = Bishop 
  | otherwise = Knight

parseColor :: Char -> Color 
parseColor c = White