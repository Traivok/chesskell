# Chesskell
Take turns with your opponent and play by writing in the extended chess notation the moves you want to make.
A wrong move makes you lose instantly, so think carefully.

## Running the game
to start the game just run:

`./chesskell`

If you want to run it on GHCi you can do

`gchi main.hs`

and then inside ghci run

`chesskell`

To start the main game loop.

#### Warning
If run the game inside ghci, you'll have a bad time using backspace. We recommend using the compiled version so you can correct your typing of the moves you intend to make.

## Compiling
`ghci main.hs -o chesskell` 
## Things to note when playing
- The syntax for a pawn promotion is as follows:
  - for a promotion with capture, you could do: Pg7xh8=N  (pawn at g7 captures piece at h8 and gets promoted to a knight)
  - for a promotion without a capture, you could do: Pg7-g8=Q (pawn at g7 advances to g8 and gets promoted to a queen)
- The rest of the syntax is pretty straightforward, here are a couple examples of valid input strings (the strings are valid, but the validity of moves is verified at runtime, since it depends on the current state of the board.):
  - Ph4-h5 _(Pawn at H4 moves to H5)_
  - Rh8-h4 _(Rook at H8 moves to H4)_
  - Ra1xa6  _(Rook at A1 captures whatever enemy piece is standing at A6)_
  - Qd2xg4 _(Queen at D2 captures whatever enemy piece is standing at G4)_

## Things we are not proud of
- moves.hs is a complete mess. It works, but we focused too much on getting it working and not on code optimization/legibility. Since the project had a short deadline, we left the moves.hs in that horrendous state.

## Things we think that can be done/enhanced
- Cleanup move.hs
- We would like to add an AI to play against the player, instead of 2 human players. We intend to use the minmax strategy counting the value of the Pieces and also evaluate for a mate / stalemate.
  - For that we tought about the following data structures, which would represent the value of the piece and how many turns would require to result in a win/draw/loss:
    - `Data Evaluation = Value Double | Wins Int | Draw Int | Losses Int`
    - `Data Heuristic a = Evaluate (a -> Evaluation)`

# Examples
### Example of a game start:
