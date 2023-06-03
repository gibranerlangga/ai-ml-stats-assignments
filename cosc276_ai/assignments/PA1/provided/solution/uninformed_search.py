
from collections import deque
from SearchSolution import SearchSolution

# you might find a SearchNode class useful to wrap state objects,
#  keep track of current depth for the dfs, and point to parent nodes
class SearchNode:
    # each search node except the root has a parent node
    # and all search nodes wrap a state object
    def __init__(self, state, parent=None):
        self.state = state
        self.parent = parent

# you might write other helper functions, too. For example,
#  I like to separate out backchaining, and the dfs path checking functions

def backchain(current_node):
  chain_path = deque([])
  while current_node.parent != None:
    chain_path.appendleft(current_node.state)
    current_node = current_node.parent
  return chain_path

def bfs_search(search_problem):
  # bfs implementation based on bfs graph-search pseudocode in the slide
  frontier = deque([])
  first_node = SearchNode(search_problem.start_state)
  frontier.append(first_node)

  explored = set([])
  solution = SearchSolution(search_problem, "BFS")

  while len(frontier) > 0:
    current_node = frontier.popleft()
    current_state = current_node.state

    if current_state not in explored:
      explored.add(current_state)
      solution.nodes_visited += 1

      if search_problem.goal_test(current_state):
        solution.path = backchain(current_node)
        return solution

      for child in search_problem.get_successors(current_state):
        child_node = SearchNode(child, current_node)
        frontier.append(child_node)

  return solution


def dfs_search(search_problem, depth_limit=100, node=None, solution=None):
    # if no node object given, create a new search from starting state
    if node == None:
        node = SearchNode(search_problem.start_state)
        solution = SearchSolution(search_problem, "DFS")

    def nested_dfs(search_problem, depth_limit=100, node=None, solution=None):
      if search_problem.goal_test(node.state):
        solution.path = backchain(node)
        return solution

      if depth_limit == 0:
        no_solution = SearchSolution(search_problem, "DFS")
        no_solution.nodes_visited = solution.nodes_visited
        return no_solution

      for child in search_problem.get_successors(node.state):
        if child not in solution.path:
          solution.nodes_visited += 1
          solution_copy = solution
          solution_copy.path.append(child)

          child_node = SearchNode(child, node)
          solution_copy = nested_dfs(search_problem, depth_limit-1, child_node, solution_copy)
          if solution_copy != None: return solution_copy
      return None
    
    # call the recursive function
    solution_dfs = nested_dfs(search_problem, depth_limit, node, solution)
    if solution_dfs == None:
      solution_dfs = SearchSolution(search_problem, solution.search_method)
      solution_dfs.nodes_visited = solution.nodes_visited
    return solution_dfs


def ids_search(search_problem, depth_limit=100):
    solution = SearchSolution(search_problem, "IDS")
    node = SearchNode(search_problem.start_state)
    current_depth = 0

    while current_depth < depth_limit:
      solution = dfs_search(search_problem, current_depth, node, solution)
      if len(solution.path) == 0:
        current_depth += 1
      else:
        return solution
    return solution
