from multiprocessing import heap
from turtle import back
from SearchSolution import SearchSolution
from heapq import heappush, heappop

class AstarNode:
    # each search node except the root has a parent node
    # and all search nodes wrap a state object

    def __init__(self, state, heuristic, parent=None, transition_cost=0):
        self.state = state
        self.heuristic = heuristic
        self.parent = parent
        self.transition_cost = transition_cost

    def priority(self):
        # the priority is the sum of the heuristic and the cost
        return self.heuristic + self.transition_cost

    # comparison operator,
    # needed for heappush and heappop to work with AstarNodes:
    def __lt__(self, other):
        return self.priority() < other.priority()


# take the current node, and follow its parents back
#  as far as possible. Grab the states from the nodes,
#  and reverse the resulting list of states.
def backchain(node):
    result = []
    current = node
    while current:
        result.append(current.state)
        current = current.parent

    result.reverse()
    return result


def astar_search(search_problem, heuristic_fn):
    start_node = AstarNode(search_problem.start_state, heuristic_fn(search_problem.start_state))
    pqueue = []
    # add start node into priority queue
    heappush(pqueue, start_node)

    solution = SearchSolution(search_problem, "Astar with heuristic " + heuristic_fn.__name__)

    visited_cost = {}
    visited_cost[start_node.state] = start_node.priority()

    no_solution_found_cnt = 0

    while len(pqueue) > 0:
        solution.nodes_visited += 1
        current = heappop(pqueue)
        if current.state in visited_cost and current.priority() > visited_cost[current.state]:
            continue

        if search_problem.goal_test(current.state) == True:
            solution.path = backchain(current)
            solution.cost = len(solution.path)
            return solution

        else:
            child_nodes = search_problem.get_successors(current.state)
            if len(child_nodes) == 0:
                no_solution_found_cnt += 0
                if no_solution_found_cnt > search_problem.numRobots:
                    break
                heappush(pqueue, current)
                continue
            no_solution_found_cnt = 0

            for child in child_nodes:
                child_node = AstarNode(child, heuristic_fn(child), current, current.transition_cost+1)
                visit_cost = child_node.priority()

                if child_node.state not in visited_cost:
                    visited_cost[child_node.state] = visit_cost
                    heappush(pqueue, child_node)
                
                # if child is in the frontier with higher child cost 
                elif visit_cost < visited_cost[child_node.state]:
                    # replace that frontier node with child node and update the cost
                    heappush(pqueue, child_node)
                    visited_cost[child_node.state] = visit_cost
                    

    return solution

