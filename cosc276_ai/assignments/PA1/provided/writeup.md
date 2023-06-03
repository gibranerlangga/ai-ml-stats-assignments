# COSC276, Fall 2022 - PA1 - Gibran Erlangga

## Problem Setup

The Chickens and Foxes problem is specifically represented in FoxProblem.py under the FoxProblem object. The object consists of few modules:
- get_successors(): to generate the list of child nodes from the current state. 
- goal_test(): to check whether the current state has reached the goal state.
- action_checker(): to define actions that can be carried out from that state, and don't lead to a state where someone is eaten

## States
Upper bound on the number of states are: 3 x 3 x 2 (chicken x fox x boat)

some rules I applied to filter out the illegal states are:
- asdsa
- asdsa
- asdsa

States are either legal, or not legal.  First, give an upper bound on the number of states, without considering legality of states.

Use a drawing program such as inkscape to draw part of the graph of states, including at least the first state, all actions from that state, and all actions from those first-reached states.  Show which of these states are legal and which aren't (for example, by using the color of the nodes).  Include and describe this figure in your report.   


Search algorithms

BFS
how-to

DFS
how-to

IDS
how-to