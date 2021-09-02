If run from TextPad, files must be compiled in the following order:
- StateNode
- UnweightedShortestPathFinder
- BreadthFirstSearchPathFinder
- Demo

=====================================================================================

TO PARAMETRIZE THE PROBLEM got to Demo.java ln. 16:

List<StateNode> path = finder.search(StateNode.getInitialStateNode(7, 7, 4), ...

Parameters:
1st the total amount of missionaries.
2nd the total amount of cannibals.
3rd the total amount of places on the boat.
