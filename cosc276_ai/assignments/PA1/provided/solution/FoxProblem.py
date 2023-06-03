class FoxProblem:
    def __init__(self, start_state=(3, 3, 1)):
        self.start_state = start_state
        self.goal_state = (0, 0, 0)
        self.total_chickens = start_state[0]
        self.total_foxes = start_state[1]
        self.possible_actions = set([])
        # i for number of chicken(s) moved left/right, j for foxes, and k for boat movement direction
        for i in [-2, -1, 0, 1, 2]:
            for j in [-2, -1, 0, 1, 2]:
                for k in [1, -1]:
                    action = (i, j, k)
                    self.possible_actions.add(action)


    def action_checker(self, state, action):
        a, b, c = state
        i, j, k = action

        # get the state on the other side
        chickens_right = self.total_chickens - a - i
        foxes_right = self.total_foxes - b - j

        # checking if number of chickens are equal or more than foxes on the left side
        if ((a+i) > 0) and ((b+j) > 0):
            if ((b+j) - (a+i) > 0):
                return False

        # checking if number of chickens are equal or more than foxes on the right side
        if ((chickens_right > 0) and (foxes_right >0)):
            if foxes_right - chickens_right > 0:
                return False

        # checking if the combination of state and action exceeds total amount of chickens/foxes on the left side
        if ((a+i) > self.total_chickens) or ((b+j) > self.total_foxes):
            return False

        # checking if the combination of state and action exceeds total amount of chickens/foxes on the right side
        if (chickens_right > self.total_chickens) or (foxes_right > self.total_foxes):
            return False

        # condition where the boat moves from right to left
        if (k == 1):
            if (c != 0): return False # the boat has to be right at first
            if (i < 0 or j < 0): return False # moving to the right is not permitted
            # should be at least 1 animal or 2 animals max to move the boat
            if ((i+j) > 2): return False 
            if ((i+j) < 1): return False 
        # condition where the boat moves from left to right
        if (k == -1):
            if (c != 1): return False # the boat has to be left at first
            if (i > 0 or j > 0): return False # moving to the left is not permitted
            # should be at least 1 animal or 2 animals max to move the boat
            if ((i+j) < -2): return False 
            if ((i+j) > -1): return False 
        return True
        
    # get successor states for the given state
    def get_successors(self, state):
        child_states = set([])
        for action in self.possible_actions:
            if self.action_checker(state, action): # evaluate all possible actions into the action checker rule
                final_state = (state[0] + action[0], state[1] + action[1], state[2] + action[2])
                child_states.add(final_state)
        return child_states


    def goal_test(self, state):
        return state == self.goal_state


    def __str__(self):
        string =  "Chickens and foxes problem: " + str(self.start_state)
        return string

## A bit of test code
if __name__ == "__main__":
    test_cp = FoxProblem((5, 5, 1))
    print(test_cp.get_successors((5, 5, 1)))
    print(test_cp)
