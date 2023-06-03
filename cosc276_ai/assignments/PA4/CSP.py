from typing import TypeVar
from math import inf
V, D = TypeVar("V"), TypeVar("D")

class CSPSolver():
    def __init__(self, var, domain, MRV=True, DH=False, AC3_flag=False):
        self.var = var
        self.domain = domain        
        self.constraints = {i: [] for i in self.var}
        self.MRV = MRV
        self.DH = DH
        self.AC3_flag = AC3_flag


    def add_constraint(self, constraint):
        for var in constraint.variables:
            self.constraints[var].append(constraint)


    def consistency_check(self, var, assignment):
        for constraint in self.constraints[var]:
            if not constraint.satisfied_check(assignment):
                return False
        return True


    def order_domain_values(self, var):
        def sort_domain(value):
            count = 0
            assignment_temp = {var: value}
            for neighbors in self.constraints[var]:
                other_var = None
                for other in neighbors.variables:
                    if other != var:
                        other_var = other
                for val in self.domain[other_var]:
                    assignment_temp[other_var] = val
                    if neighbors.satisfied_check(assignment_temp):
                        count += 1
            return count

        var_temp = self.domain[var]
        var_temp.sort(key=sort_domain, reverse=True)
        return var_temp


    def backtrack(self, assignment={}, LCV=False):
        # if assignment is complete then return assignment
        if len(assignment) == len(self.var):
            return assignment

        # get unassigned vars
        unassigned = [var for var in self.var if var not in assignment]

        first = unassigned[0]

        if self.MRV:
            first = self.MRVvar(unassigned)
        elif self.DH:
            first = self.DHvar(unassigned)

        if LCV:
            self.domain[first] = self.order_domain_values(first)

        # recursive
        for val in self.domain[first]:
            local_assign = assignment.copy()
            local_assign[first] = val
            if self.consistency_check(first, local_assign):
                if self.AC3_flag:
                    self.AC3()
                result = self.backtrack(local_assign, LCV)
                if result != None:
                    return result
        return None


    def MRVvar(self, unassigned):
        '''
        a heuristic that chooses the variable with the fewest legal values.
        '''
        shortest_length, shortest_var = inf, None

        for var in unassigned:
            if len(self.domain[var]) < shortest_length:
                shortest_var = var
                shortest_length = len(self.domain[var])
        return shortest_var

    def DHvar(self, unassigned):
        '''
        a heuristic that chooses the variable with the most constraints on remaining variables.
        '''
        max_degree, max_var = -1, None

        for var in unassigned:
            if len(self.constraints[var]) > max_degree:
                max_var = var
                max_degree = len(self.constraints[var])
        return max_var


    def AC3(self):
        queue = []
        for var in self.constraints:
            for constraint_arc in self.constraints[var]:
                queue.append((var, constraint_arc))

        while len(queue) > 0:
            x_i, constraint = queue.pop()
            x_j = None

            for i in constraint.variables:
                if i != x_i:
                    x_j = i

            if self.revise(x_i, x_j, constraint):
                if len(self.domain[x_i]) == 0:
                    return False
                for arc in self.constraints[x_i]:
                    x_k = None
                    for i in arc.variables:
                        if i != x_i:
                            x_k = i
                    if arc != constraint:
                        queue.append((x_k, constraint))
        return True
        

    def revise(self, x_i, x_j, constraint):
        revised = False

        for x in self.domain[x_i]:
            assignment_temp = {x_i: x}
            satisfiable_y = False
            for y in self.domain[x_j]:
                assignment_temp[x_j] = y
                if constraint.satisfied_check(assignment_temp):
                    satisfiable_y = True
            if satisfiable_y == False:
                self.domain[x_i].remove(x)
                revised = True
        return revised
