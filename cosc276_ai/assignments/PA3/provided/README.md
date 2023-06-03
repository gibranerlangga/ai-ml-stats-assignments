# COSC276 Fall Term 2022 - PA3 - Gibran Erlangga

## How to run the code
Go to *test_chess.py* and you will see two variables: player1 and player2. You can set up the core algorithm to run each player with the following options: RandomAI, HumanPlayer, MinimaxAI or AlphaBetaAI. By default, player1 is set to MinimaxAI(3) and player2 is set to RandomAI(). Specifically for MinimaxAI and AlphaBetaAI object, you will need to specify the maximum depth as the only parameter to the object (only accepts positive integer). In essence, the larger the value, the more time needed for the algorithm to decide on the next move as it computes and compares more move options. 

### Minimax algorithm
To run the minimax algorithm, you can set the player1 or player2 to *MinimaxAI(max_depth)* with your desired *max_depth* value.

### Alpha-beta pruning algorithm
To run the alpha-beta algorithm, you can set the player1 or player2 to *AlphaBetaAI(max_depth)* with your desired *max_depth* value.