import chess
from math import inf


class AlphaBetaAI():
    def __init__(self, depth):
        self.depth_limit = depth
        self.states_visited = 0
        self.alpha_beta_calls = 0
        self.transposeTable = {}


    def choose_move(self, board):
        temp_board = board
        self.alpha_beta_calls += 1
        return self.iterative_deepening(temp_board)


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


    def alphabeta(self, board, depth, alpha, beta, current_max):
        self.states_visited += 1

        if hash(str(board)) in self.transposeTable:
            temp = self.transposeTable[hash(str(board))]
            if (self.depth_limit - depth) <= temp[1]:
                return temp[0]
        elif self.cutoff_test(board, depth, current_max):
            return self.get_utility_score(board)

        if board.turn == chess.BLACK:
            return self.max_value(board, depth, alpha, beta, current_max)
        else:
            return self.min_value(board, depth, alpha, beta, current_max)


    def max_value(self, board, depth, alpha, beta, curr_max):
        v = inf
        depth += 1
        for moves in self.sort_board(board):
            board.push(moves)
            v = min(v, self.alphabeta(board, depth, alpha, beta, curr_max))
            beta = min(beta, v)
            board.pop()
            if beta <= alpha:
                break
        self.transposeTable[hash(str(board))] = [v, self.depth_limit-depth]
        return beta


    def min_value(self, board, depth, alpha, beta, curr_max):
        v = -inf
        depth += 1
        for moves in self.sort_board(board):
            board.push(moves)
            v = max(v, self.alphabeta(board, depth, alpha, beta, curr_max))
            alpha = max(alpha, v)
            board.pop()
            if beta <= alpha:
                break
        self.transposeTable[hash(str(board))] = [v, self.depth_limit-depth]
        return alpha


    def sort_board(self, board):
        legal_moves = list(board.legal_moves)
        moves_util_score = {}
        for moves in legal_moves:
            board.push(moves)
            moves_util_score[moves] = self.get_utility_score(board)
            board.pop()
        def map_moves_scores(move):
            return moves_util_score[move]
        legal_moves.sort(reverse=True, key=map_moves_scores)
        return legal_moves


    def iterative_deepening(self, board):
        current_max = 0
        largest_val = -inf
        best_move = list(board.legal_moves)[0]
        temp = board
        while current_max < self.depth_limit:
            self.states_visited = 0
            board = temp
            legal_moves = self.sort_board(board)
            for move in legal_moves:
                board.push(move)
                current_val = self.alphabeta(board, depth=0, alpha=-inf, beta=inf, current_max=current_max)
                if current_val > largest_val:
                    largest_val = current_val
                    best_move = move
                if largest_val == inf:
                    return best_move
                board.pop()
            current_max += 1
        print('AlphaBetaAI move:', best_move)
        
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