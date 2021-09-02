package lab1;

import java.util.ArrayDeque;
import java.util.Collections;
import java.util.Deque;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Predicate;

/**
 * This class implements the breadth-first search path finder.
 * Initial author: Rodion Efremov the Finnish Open-Source Computer Scientist
 * Edited by: Nikolaos Lintas from University of Sheffield
 */
 
public class BreadthFirstSearchPathFinder<N extends Iterable<N>> implements UnweightedShortestPathFinder<N> {

	/**
	 * Searches for a shortest path using breadth-first search.
	 * 
	 * @param source          the source node.
	 * @param targetPredicate the target node predicate.
	 * @return the shortest path from source to the first node that passes the
	 *         target node predicate.
	 */
	@Override
	public List<N> search(N source, Predicate<N> targetPredicate) {
		Objects.requireNonNull(source, "The source node is null.");
		Objects.requireNonNull(targetPredicate, "The target predicate is null.");

		Map<N, N> parentMap = new HashMap<>();
		Deque<N> queue = new ArrayDeque<>();

		parentMap.put(source, null);
		queue.addLast(source);

		while (!queue.isEmpty()) {
			N current = queue.removeFirst();

			if (targetPredicate.test(current)) {
				return tracebackPath(current, parentMap);
			}

			for (N child : current) {
				if (!parentMap.containsKey(child)) {
					parentMap.put(child, current);
					queue.addLast(child);
				}
			}
		}

		return Collections.<N>emptyList();
	}
}