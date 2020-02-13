This is my old Haskell-implementation of the canfield-style (hard; casino variant) of solitaire.

It was developed back in 2011 and 2012 as a personal project but ended in some kind of code-golf.

The main goals was simplicity. There are (intentionally) no animations and it is controlled by keyboard only.

It still compiles but uses the legacy SDL (1.x). Using cabal a simple

    cabal v2-run

should bring it to life.

## Control

Each card stack has a key assigned to it.
Moving a card or a whole stack is done by tapping the key of the "from"-stack followed by the "destination"-key.

The keys are assigned as followed:

| Key   | Stack        |
| :---: | ------------ |
|   a   | tableau 1    |
|   s   | tableau 2    |
|   d   | tableau 3    |
|   f   | tableau 4    |
|   q   | foundation 1 |
|   w   | foundation 2 |
|   e   | foundation 3 |
|   r   | foundation 4 |
|   t   | reserve      |
|   g   | talon        |
| space | draw 3 cards from the hand |
|       | (or take back the talon to the hand) |
|   n   | new game     |
|  esc  | close game   |

Where foundations are the upper row of stacks to fill, tableaus are the "main stacks" the reserve is a special stack on the left that's accessed card by card (the main feature of the canfied style) and the talon the stack on the right is restocked by the hand below.

