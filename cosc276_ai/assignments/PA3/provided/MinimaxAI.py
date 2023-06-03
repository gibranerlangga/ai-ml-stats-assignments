import chess
from math import inf

class MinimaxAI():
    def __init__(self, depth):
        self.depth_limit = depth
        self.states_visited = 0
        self.minimax_calls = 0


    def choose_move(self, board):
        temp_board = board
        self.minimax_calls += 1
        choice = self.minimax(temp_board)
        return choice


    def cutoff_test(self, board, depth, depth_limit):
        # check for legality of the states
        if board.is_checkmate():
            return True
        elif board.is_stalemate():
            return True
        elif depth >= depth_limit:
            return True
        return False


    def get_utility_score(self, board):
        max_player = (board.turn == chess.BLACK)
        val = 0

        if board.is_checkmate():
            if max_player:
                val = inf
            else:
                val = -inf
        elif board.is_stalemate():
            val = 0
        else:
            val = self.get_move_score(board)

        return val


    def max_value(self, board, depth, curr_max):
        self.states_visited += 1

        if self.cutoff_test(board, depth, curr_max):
            return self.get_utility_score(board)

        v = -inf
        depth += 1
        for move in set(board.legal_moves):
            board.push(move)
            v = max(v, self.min_value(board, depth, curr_max))
            board.pop()
        return v


    def min_value(self, board, depth, curr_max):
        self.states_visited += 1

        if self.cutoff_test(board, depth, curr_max):
            return self.get_utility_score(board)

        v = inf
        depth += 1
        for move in set(board.legal_moves):
            board.push(move)
            v = min(v, self.max_value(board, depth, curr_max))
            board.pop()
        return v


    def minimax(self, board):
        # variables initialization
        current_val, current_max = 0, 0
        largest_val = -inf
        best_move = list(board.legal_moves)[0]
        legal_moves = set(list(board.legal_moves))
        temp = board

        while current_max < self.depth_limit:
            self.states_visited = 0
            board = temp
            for move in legal_moves:
                board.push(move)
                current_val = self.min_value(board, 0, current_max)
                if current_val > largest_val:
                    largest_val = current_val
                    best_move = move
                board.pop()
            current_max += 1
        print('minimaxAI move:', best_move)

        return best_move
        

    def get_move_score(self, board):
        # calculate score by the availability of its piece types
        white_p, black_p = len(board.pieces(chess.PAWN, chess.WHITE)), len(board.pieces(chess.PAWN, chess.BLACK))
        white_k, black_k = len(board.pieces(chess.KNIGHT, chess.WHITE)), len(board.pieces(chess.KNIGHT, chess.BLACK))
        white_b, black_b = len(board.pieces(chess.BISHOP, chess.WHITE)), len(board.pieces(chess.BISHOP, chess.BLACK))
        white_r, black_r = len(board.pieces(chess.ROOK, chess.WHITE)), len(board.pieces(chess.ROOK, chess.BLACK))
        white_q, black_q = len(board.pieces(chess.QUEEN, chess.WHITE)), len(board.pieces(chess.QUEEN, chess.BLACK))
        white_k, black_k = len(board.pieces(chess.KING, chess.WHITE)), len(board.pieces(chess.KING, chess.BLACK))

        score = (white_p - black_p) * 1 \
                + (white_k - black_k) * 3 \
                + (white_b - black_b) * 3 \
                + (white_r - black_r) * 5 \
                + (white_q - black_q) * 9 \
                + (white_k - black_k) * 100
        return score