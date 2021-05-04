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

# Examples - Chess Openings
### Queen's Gambit:
> _One of the fundamental variations of 1.d4 is the Queen's Gambit. White immediately strikes at Black's central pawn from the side. Although the c4 pawn is not guarded, this is not a 'real' gambit, since White can always get the pawn back if he wants._
- [W] Pd2-d4
- [B] Pd7-d5
- [W] Pc2-c4
<img width="324" alt="Screen Shot 2021-05-04 at 18 43 58" src="https://user-images.githubusercontent.com/24229855/117073886-f5cb0a00-ad08-11eb-81c8-afcafb403e7d.png">

### Ruy Lopez
> _The Ruy Lopez, also known as the Spanish opening, develops the kingside pieces quickly and puts pressure on the center._
- [W] Pe2-e4
- [B] Pe7-e5
- [W] Ng1-f3
- [B] Nb8-c6
- [W] Bf1-b5
- [B] (Most popular, but not a rule) Pa7-a6
<img width="339" alt="Screen Shot 2021-05-04 at 18 49 47" src="https://user-images.githubusercontent.com/24229855/117074367-96b9c500-ad09-11eb-83a4-f65a4017c4d5.png">

<img width="295" alt="Screen Shot 2021-05-04 at 18 50 02" src="https://user-images.githubusercontent.com/24229855/117074373-991c1f00-ad09-11eb-860b-fcf611ad4e69.png">

# Example - Full Game
At the root of this project there is a file `samplegame.txt`. It contains a chess game of 49 moves where the white pieces win. You can type the game line by line to see it developing slowly, or you can just feed the input to the game by doing:
`./chesskell < samplegame.txt`. The final board it should produce should look like this:

<img width="162" alt="Screen Shot 2021-05-04 at 19 35 19" src="https://user-images.githubusercontent.com/24229855/117078244-e602f400-ad0f-11eb-9af3-5903ae33cfb4.png">

As we can see there is a checkmate on the black pieces' king on g8.
