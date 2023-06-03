from MazeworldProblem import MazeworldProblem
from Maze import Maze
from astar_search import astar_search

# null heuristic, useful for testing astar search without heuristic (uniform cost search).
def null_heuristic(state):
    return 0

def solve_maze(maze, goalLocations):
    # initialize maze
    problem = Maze(maze)
    maze_problem = MazeworldProblem(problem, goalLocations)

    # run astar search with manhattan heuristic
    result = astar_search(maze_problem, maze_problem.manhattan_heuristic)
    print(result)
    maze_problem.animate_path(result.path)

## Test problems
## provided
# solve_maze("maze2.maz", (3,0))
# solve_maze("maze3.maz", (1,4,1,3,1,2))

## problems
# solve_maze("maze1_gibran.maz", (0,6,1,6,2,6))
# solve_maze("maze2_gibran.maz", (1,5,3,5,2,5))
# solve_maze("maze3_gibran.maz", (19,0,20,0,20,1))
# solve_maze("maze4_gibran.maz", (9,0,10,0))
# solve_maze("maze5_gibran.maz", (90,0))