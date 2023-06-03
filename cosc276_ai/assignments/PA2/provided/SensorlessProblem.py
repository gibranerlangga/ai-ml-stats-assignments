from re import S
from Maze import Maze
from time import sleep

class SensorlessProblem:
    def __init__(self, maze):
        self.maze = maze
        self.actions = set([(-1,0), (1,0), (0,-1), (0,1)])
        self.actions_str = {(-1, 0): "west",
                            (1, 0): "east",
                            (0, -1): "south",
                            (0, 1): "north"}

        # add all empty floors into the start state
        self.start_state = []
        for i in range(self.maze.width):
            for j in range(self.maze.height):
                if self.maze.is_floor(i, j):
                    self.start_state.append((i,j))
        self.start_state = tuple(self.start_state)


    def get_successors(self, state):
        child_nodes = set([])

        for action in self.actions:
            x_action, y_action = action
            next_state = set([])
            for current_state in state:
                x, y = current_state
                x_updated = x + x_action
                y_updated = y + y_action
            
                # check if its legal
                if self.maze.is_floor(x_updated, y_updated):
                    next_state.add((x_updated, y_updated))
                else:
                    next_state.add(current_state)
            child_nodes.add(tuple(next_state))
        return tuple(child_nodes)


    def get_states_path(self, state, next_state):
        for action in self.actions:
            x_action, y_action = action
            state_temp = set([])
            for current_state in state:
                x, y = current_state
                x_updated = x + x_action
                y_updated = y + y_action

                if self.maze.is_floor(x_updated, y_updated):
                    state_temp.add((x_updated, y_updated))
                else:
                    state_temp.add(current_state)
            if tuple(state_temp) == next_state:
                return self.actions_str[(x_action,y_action)]


    def backchain_path(self, backchain):
        path = []
        for i in range(len(backchain)-1):
            path.append(self.get_states_path(backchain[i], backchain[i+1]))
        return path


    def heuristic(self, state):
        # in worst case, the robot needs to travel on every possible floor to understand where it is.
        # best case, it will need to do len(state) - 1 to fully understand where it is.
        return len(state) - 1


    def goal_test(self, state):
        return len(state) == 1


    def animate_path(self, path):
        # reset the robot locations in the maze
        self.maze.robotloc = tuple(self.start_state)

        for state in path:
            print(str(self))
            self.maze.robotloc = []
            for i in state:
                if len(i) > 1:
                    self.maze.robotloc.append(i[0])
                    self.maze.robotloc.append(i[1])
            self.maze.robotloc = tuple(self.maze.robotloc)
            sleep(1)
            
            print(str(self.maze))


    def __str__(self):
        string =  "Blind robot problem: "
        return string


## A bit of test code
if __name__ == "__main__":
    test_maze3 = Maze("maze3.maz")
    test_problem = SensorlessProblem(test_maze3)
    print(test_maze3)
