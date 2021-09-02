package lab1;

import java.util.List;

/**
 * This class is a demonstration.
 * Initial author: Rodion Efremov the Finnish Open-Source Computer Scientist
 * Edited by: Nikolaos Lintas from University of Sheffield
 */

public class Demo {

	public static void main(String[] args) {
		UnweightedShortestPathFinder<StateNode> finder = new BreadthFirstSearchPathFinder<>();
		UnweightedShortestPathFinder<StateNode> finder2 = new DepthFirstSearch<>();

		long startTime = System.currentTimeMillis();

		//BFS
		// initialStateNode format (missionaries, cannibals, boat capacity)
		List<StateNode> path = finder.search(StateNode.getInitialStateNode(7, 7, 3), (StateNode node) -> {
			return node.isSolutionState();
		});
// DFS
//		List<StateNode> path = finder2.search(StateNode.getInitialStateNode(7, 7, 4), (StateNode node) -> {
//			return node.isSolutionState();
//		});

		long endTime = System.currentTimeMillis();

		System.out.println("Duration: " + (endTime - startTime) + " milliseconds.");

		int fieldLength = ("" + path.size()).length();

		if (path.isEmpty()) {
			System.out.println("No solution.");
		} else {

			int i = 0;
			for (StateNode step : path) {
				System.out.printf("State %" + fieldLength + "d: %s\n", ++i, step);
			}
		}
	}
}