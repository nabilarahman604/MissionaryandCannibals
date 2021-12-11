
For this problem, the boat is considered to transport passengers from left bank to the right bank. Presence of boat on the left and right bank of the river is represented by boat_pos values of 0 and 1 respectively. Thus according to the given input configurations of 15 missionaries, 15 cannibals and a boat capacity of 6, the start state will be represented as equation and the goal state is represented as equation.


Algorithm:
The steps followed by the algorithm can be stated as follows:

  1.	Initialize start state and goal state.
  2.	Initialize open list which contains nodes which are considered for expansion at every iteration, to null.
  3.	Initialize visited state list which contains already expanded states to null.
  4.	Push the start state into the open list.
  5.	Pop the best element from the open list using heuristic function and set it as the current state.
  6.	If the current state is the goal state then return success and print the path from start state to goal state.
  7.	Generate successor states for the current states and push only those states into the open list which are not
        present in the visited list.
  8.	Push current state into the visited list.
  9.	Go to step 5 if the open list is not empty, else return failure.
  
  
  
How to run:
Edit and save the file to enter start and goal states in the format mentioned later. Run the file using the command "clisp mcp.lisp" from the terminal.


Output:
The output shows the sequence of states expanded to reach the goal state from the initial state. Additionally, loop count and open list length is shown for every iteration for debugging purposes.
