import org.junit.Test;
import static org.junit.jupiter.api.Assertions.*;
import java.util.Arrays;

public class DAGSortTest {

  @Test
  // Testing a null DAG given
  public void testNullDAG() {
    assertThrows(NullPointerException.class, () -> DAGSort.sortDAG(null), "Null instance passed");
  }

  @Test
  // Testing an empty DAG
  public void testEmptyDAG() {
    boolean testResult = false;
    int[] correct = null;

    int[][] graph = new int[][] {};
    int[][] expected = new int[][] {};

    try {
      correct = DAGSort.sortDAG(graph);
    } catch (Exception ignored) {
    }

    if (correct.length == expected.length) {
      testResult = true;
    }

    assertTrue(testResult);
  }

  @Test
  // Checks the output of a correct DAG.
  public void testDAGOutput() {
    boolean testResult = false;
    int[] correct = null;

    int[][] graph = new int[6][];
    graph[0] = new int[] {};
    graph[1] = new int[] {};
    graph[2] = new int[] {3};
    graph[3] = new int[] {1};
    graph[4] = new int[] {0, 1};
    graph[5] = new int[] {0, 2};

    int[][] expected = new int[13][];

    expected[0] = new int[] {4, 5, 0, 2, 3, 1};
    expected[1] = new int[] {4, 5, 2, 0, 3, 1};
    expected[2] = new int[] {4, 5, 2, 3, 0, 1};
    expected[3] = new int[] {4, 5, 2, 3, 1, 0};
    expected[4] = new int[] {5, 2, 3, 4, 0, 1};
    expected[5] = new int[] {5, 2, 3, 4, 1, 0};
    expected[6] = new int[] {5, 2, 4, 0, 3, 1};
    expected[7] = new int[] {5, 2, 4, 3, 0, 1};
    expected[8] = new int[] {5, 2, 4, 3, 1, 0};
    expected[9] = new int[] {5, 4, 0, 2, 3, 1};
    expected[10] = new int[] {5, 4, 2, 0, 3, 1};
    expected[11] = new int[] {5, 4, 2, 3, 0, 1};
    expected[12] = new int[] {5, 4, 2, 3, 1, 0};

    try {
      correct = DAGSort.sortDAG(graph);
    } catch (Exception ignored) {
    }

    for (int[] expectedArray : expected) {
      if (Arrays.equals(expectedArray, correct)) {
        testResult = true;
        break;
      }
    }
    assertTrue(testResult);
  }

  @Test
  // Testing that the DAG has no negative nodes
  public void testNegativeNode() {
    int[][] graph = new int[3][];
    graph[0] = new int[] {1, 2};
    graph[1] = new int[] {2};
    graph[2] = new int[] {-1}; // Inserting the negative value

    assertThrows(
        InvalidNodeException.class,
        () -> DAGSort.sortDAG(graph),
        "Nodes must be from 0 to N with no negative nodes");
  }

  @Test
  // Testing that the DAG has no node bigger than the size of the DAG
  public void testOutOfBoundsNode() {
    int[][] graph = new int[3][];
    graph[0] = new int[] {1, 2};
    graph[1] = new int[] {2};
    graph[2] = new int[] {3};

    assertThrows(
        InvalidNodeException.class,
        () -> DAGSort.sortDAG(graph),
        "Nodes must be labelled from 0 to N - 1");
  }

  @Test
  // A singleton DAG is cyclic so it's cyclinc and an error has to be thrown
  public void testSingletonDAG() {
    int[][] singletonGraph = new int[1][];
    singletonGraph[0] = new int[] {0};

    assertThrows(
        CycleDetectedException.class,
        () -> DAGSort.sortDAG(singletonGraph),
        "Cyclic graphs are not allowed.");
  }

  @Test
  // A cyclic graph shouldn't pass
  public void testCyclicGraph() {
    int[][] cyclicGraph = new int[3][];
    cyclicGraph[0] = new int[] {1, 2};
    cyclicGraph[1] = new int[] {2};
    cyclicGraph[2] = new int[] {2};

    assertThrows(
        CycleDetectedException.class,
        () -> DAGSort.sortDAG(cyclicGraph),
        "Cyclic graphs are not allowed.");
  }

  @Test
  // Test a node with all other nodes being it's child, no errors should be thrown
  public void testMultipleChildren() {
    int[] correct = null;
    boolean testResult = false;

    int[][] graph = {{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}, {}, {}, {}, {}, {}, {}, {}, {}, {}, {}};

    try {
      correct = DAGSort.sortDAG(graph);
    } catch (Exception ignored) {
    }

    if (correct[0] == 0) {
      testResult = true;
    }

    assertTrue(testResult);
  }
}
