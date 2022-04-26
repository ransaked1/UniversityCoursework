import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;

// The broken one
public class DAGSortInvalid {

	public static int[] sortDAG(int[][] edges) throws CycleDetectedException, InvalidNodeException {
		return new int[] {0};
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