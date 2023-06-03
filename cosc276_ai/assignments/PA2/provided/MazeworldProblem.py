from Maze import Maze
from time import sleep

class MazeworldProblem:

    def __init__(self, maze, goal_locations):
        # maze, start state, and goal state initialization
        self.maze = maze
        self.start_state, self.goal_locations = [], []
        
        # actions space: (north, south, west, east)
        # self.actions = set([(0,1), (0,-1), (-1,0), (1,0)])
        self.actions = set([(-1,0), (1,0), (0,-1), (0,1)])
        self.numRobots = len(self.maze.robotloc) // 2
        self.current_robot_idx = 0

        # update start state
        for i in range(0, len(self.maze.robotloc), 2):
            loc = (self.maze.robotloc[i], self.maze.robotloc[i+1])
            self.start_state.append(loc)
        self.start_state = tuple(self.start_state)

        # update goal locations
        for i in range(0, len(goal_locations), 2):
            goal = (goal_locations[i], goal_locations[i+1])
            self.goal_locations.append(goal)
        self.goal_locations = tuple(self.goal_locations)
        

    def __str__(self):
        string =  "Mazeworld problem: "
        return string


    def get_successors(self, state):
        x_current, y_current = state[self.current_robot_idx]
        child_nodes = set([])
        
        for action in self.actions:
            action_x, action_y = action
            x_updated = x_current + action_x
            y_updated = y_current + action_y

            # check if the updated state is an empty floor
            if self.state_validator(state, x_updated, y_updated):
                legal_state = list(state)
                legal_state[self.current_robot_idx] = (x_updated, y_updated)
                child_nodes.add(tuple(legal_state))
        self.current_robot_idx = (self.current_robot_idx + 1) % self.numRobots

        return child_nodes


    def manhattan_heuristic(self, state):
        distance = 0 # distance initialization
        for i in range(len(state)):
            x_goal, y_goal = self.goal_locations[i]
            x_current, y_current = state[i]
            distance += abs(x_goal - x_current) + abs(y_goal - y_current)
        return distance


    def state_validator(self, state, x, y):
        if self.maze.is_floor(x, y):
            for i in range(len(state)):
                if state[i] == (x, y) and i != self.current_robot_idx:
                    return False
            return True
        # false if state is not on the floor
        return False


    def goal_test(self, state):
        return state == self.goal_locations


    def animate_path(self, path):
        # reset the robot locations in the maze
        self.maze.robotloc = tuple(self.start_state[1:])

        for state in path:
            print(str(self))
            state_new = []
            for i in state:
                x, y = i
                state_new.append(x)
                state_new.append(y)
                self.maze.robotloc = tuple(state_new)
            sleep(1)

            print(str(self.maze))


## A bit of test code. You might want to add to it to verify that things
#  work as expected.

if __name__ == "__main__":
    test_maze3 = Maze("maze3.maz")
    test_mp = MazeworldProblem(test_maze3, (1, 4, 1, 3, 1, 2))
    print('maze:', test_maze3)
    print(test_mp.start_state)
    print(test_mp.get_successors(test_mp.start_state))