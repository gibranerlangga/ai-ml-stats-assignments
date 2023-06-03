from typing import Generic, TypeVar
V, D = TypeVar("V"), TypeVar("D")


class Constraint(Generic[V, D]):
    def __init__(self, variables):
        self.variables = variables


class MapConstraints(Constraint[str, str]):
    def __init__(self, state1, state2):
        super().__init__([state1, state2])
        self.state1 = state1
        self.state2 = state2

    def satisfied_check(self, assignment):
        if self.state1 not in assignment or self.state2 not in assignment:
            return True
        
        # check whether they have different assignments
        elif assignment[self.state1] != assignment[self.state2]:
            return True
        return False

class CircuitConstraints(Constraint[tuple, tuple]):
    def __init__(self, height, width):
        super().__init__([height, width])
        self.height = height
        self.width = width
    
    def satisfied_check(self, assignment):
        pass