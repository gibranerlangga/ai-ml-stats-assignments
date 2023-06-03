# COSC276 Fall Term 2022 - PA2 - Gibran Erlangga

## How to run the code
### Mazeworld problem

Open the *test_mazeworld.py* file and you will see the *solve_maze()* function with two parameters: *maze* (filename of the maze, in string) and *goalLocations* (tuple of final goal location on each robot in the maze). A sample valid input would be **solve_maze("maze1_gibran.maz", (0,6,1,6,2,6))**. When you run this function, it will execute the A-star search with manhattan heuristic and plot the result for each step. Run the *test_mazeworld.py* in your code editor and uncomment the problem maze of your interest between line 25-29 (if nothing is changed, it will run line 25).

### Sensorless problem
Open the *test_sensorless.py* file and you will see the *solve_sensorless_maze(maze)* function with one parameter: *maze* (filename of the maze, in string). A sample valid input would be **solve_sensorless_maze("sensorless1.maz")**. When you run this function, it will execute A-start search with the stated heuristic in the report. Run the *test_sensorless.py* in your code editor and uncomment the sensorless problem of your interest between line 30-33 (if nothing is changed, if will run line 31).