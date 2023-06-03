# You write this:
from SensorlessProblem import SensorlessProblem
from Maze import Maze

from astar_search import astar_search

# null heuristic, useful for testing astar search without heuristic (uniform cost search).
def null_heuristic(state):
    return 0


def solve_sensorless_maze(maze):
    maze_problem = Maze(maze)
    problem = SensorlessProblem(maze_problem)
    result = astar_search(problem, problem.heuristic) # get shortest path to localize at goal
    problem.animate_path(result.path)
    result.path = problem.backchain_path(result.path) # convert states into actions
    result.cost = len(result.path) # update path cost
    # show result path
    print(result)

solve_sensorless_maze("sensorless1_gibran.maz")
# solve_sensorless_maze("sensorless2_gibran.maz")
# solve_sensorless_maze("sensorless3_gibran.maz")