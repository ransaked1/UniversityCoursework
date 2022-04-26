import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

// The correct DAGSort
public class DAGSort {

	/**
	 * This method takes a directed acyclic graph as input and returns a
	 * topological sort of the nodes. Nodes are assumed to be labelled from 0 to
	 * (number of nodes) - 1.
	 *
	 *
	 * @param edges
	 *            An array representing the edges of the graph. Each row
	 *            contains the immediate out-neighbours of a particular node.
	 *            E.g., edges[0] = {1,2,5} means there are edges from node 0 to
	 *            nodes 1, 2 and 5.
	 * @return A valid topological order of the nodes, i.e., every node must
	 *         appear before its out-neighbours in this order.
	 * @throws CycleDetectedException
	 *             If the input graph has cycles.
	 * @throws NullPointerException
	 *             If null is provided as a parameter.
	 * @throws InvalidNodeException
	 *             If the edges parameter contains edges that are not labelled
	 *             from 0 to (edges.length - 1)
	 */
	public static int[] sortDAG(int[][] edges)
			throws CycleDetectedException, InvalidNodeException {
		if (edges == null) {
			throw new NullPointerException("Edges can't be null");
		}
		for (int[] row : edges) {
			for (int i : row) {
				if (i < 0 || i >= edges.length) {
					throw new InvalidNodeException(
							"Edges array refers to invalid node: " + i);
				}
			}
		}
		List<Integer> reverseSorted = new ArrayList<Integer>(edges.length);
		boolean[] visiting = new boolean[edges.length];
		boolean[] visited = new boolean[edges.length];
		Arrays.fill(visiting, false);
		Arrays.fill(visited, false);
		for (int i = 0; i < edges.length; i++) {
			if (!visited[i]) {
				visit(i, edges, visiting, visited, reverseSorted);
			}
		}
		int[] sorted = new int[edges.length];
		for (int i = 0; i < edges.length; i++) {
			sorted[i] = reverseSorted.get(reverseSorted.size() - 1 - i);
		}
		return sorted;
	}

	/**
	 * Carry out a (recursive) depth-first search from this node.
	 *
	 * @param i
	 *            The index of the starting node.
	 * @param edges
	 *            The edges of the graph.
	 * @param visiting
	 *            The nodes that are currently being visited as part of the DFS.
	 * @param visited
	 *            The nodes that have already been visited.
	 * @param reverseSorted
	 *            The final topological order (in reverse).
	 * @throws CycleDetectedException
	 *             If the DFS encounters a cycle.
	 */
	private static void visit(int i, int[][] edges, boolean[] visiting,
														boolean[] visited, List<Integer> reverseSorted)
			throws CycleDetectedException {
		if (visiting[i]) {
			throw new CycleDetectedException();
		}
		if (!visited[i]) {
			visiting[i] = true;
			for (int outNeighbour : edges[i]) {
				visit(outNeighbour, edges, visiting, visited, reverseSorted);
			}
			visiting[i] = false;
			visited[i] = true;
			reverseSorted.add(i);
		}
	}

}

class CycleDetectedException extends Exception {
	public CycleDetectedException() {
		super();
	}
}

class InvalidNodeException extends Exception {
	public InvalidNodeException() {
		super();
	}

	public InvalidNodeException(String s) {
		super(s);
	}
}