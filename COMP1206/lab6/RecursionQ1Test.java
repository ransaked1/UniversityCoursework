import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.File;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.Test;

import uk.ac.soton.ecs.comp1206.labtestlibrary.datastructure.Tree;
import uk.ac.soton.ecs.comp1206.labtestlibrary.io.FileSystemAccess;
import uk.ac.soton.ecs.comp1206.labtestlibrary.io.StringUtil;
import uk.ac.soton.ecs.comp1206.labtestlibrary.parse.JavaFile;
import uk.ac.soton.ecs.comp1206.labtestlibrary.recursion.RecursionChecker;
import uk.ac.soton.ecs.comp1206.labtestlibrary.recursion.Tuple;

/**
 *
 * Tests for the Lab of recursion.
 * Question 1: MinInt, MinTree.
 *
 * @author jan
 *
 */
class RecursionQ1Test {

	/**
	 * Test that the correct value is returned for the exercise to recursively find
	 * the minimum of an array. Tests the code on several arrays.
	 */
	@Test
	void MinIntCorrectValueTest() {
		MinInt minInt = new MinInt();
		List<Tuple<List<Integer>, Integer>> testData = getMinValueTestData();
		Collections.shuffle(testData);
		for (Tuple<List<Integer>, Integer> testDatum : testData) {
			int[] numbers = testDatum.getFirstValue().stream().mapToInt(i -> i).toArray();
			int foundMin = minInt.findMin(numbers);
			int expected = testDatum.getSecondValue();
			String failMessage = "Wrong minimum in array (" + StringUtil.arrayAsString(numbers) + ").";
			assertEquals(expected, foundMin, failMessage);
		}
	}

	/**
	 * Test that no loops where used in the exercise to recursively find the minimum
	 * of an array.
	 */
	@Test
	void minIntNoLoopsTest() {
		checkIfBadStatementsWhereUsed("MinInt");
	}

	/**
	 * Test that the correct value is returned for the exercise to recursively find
	 * the minimum of an array. Tests the code on several trees.
	 */
	@Test
	void isMinTreeCorrectValueTest() {
		List<Tuple<Tree, Integer>> testData = getMinTreeTestData();
		Collections.shuffle(testData);
		for (Tuple<Tree, Integer> testDatum : testData) {
			Tree testTree = testDatum.getFirstValue();
			int expected = testDatum.getSecondValue();
			MinTree minTree = new MinTree();
			int result = minTree.findMin(testTree);
			String failMessage = "Wrong minimum in tree (" + StringUtil.printTree(testTree) + ").";
			assertEquals(expected, result, failMessage);
		}
	}

	/**
	 * Test that no loops where used in the exercise to recursively find the minimum
	 * of an tree.
	 */
	@Test
	void minTreeNoLoopsTest() {
		checkIfBadStatementsWhereUsed("MinTree");
	}

	/**
	 * Test a class file if it contains any types of loops. The file is searched for
	 * based on the class name.
	 *
	 * @param className The name of the class to test.
	 */
	private void checkIfBadStatementsWhereUsed(String className) {
		File currentAbsolutePath = Paths.get("").toAbsolutePath().toFile();
		Collection<File> files = FileSystemAccess.findJavaFile(currentAbsolutePath, className);
		if (files.isEmpty()) {
			fail("Source file not found. Ensure that the test is run upwards in the directory tree of the source file of MinInt.");
		}
		String filePath = files.iterator().next().getAbsolutePath();
		try {
			JavaFile javaFile = new JavaFile(filePath);
			RecursionChecker badStatementChecker = javaFile.runCheckForBadStatements(className);
			boolean foundForLoop = badStatementChecker.foundForLoop();
			boolean foundWhileLoop = badStatementChecker.foundWhileLoop();
			boolean foundForEachLoop = badStatementChecker.foundForEachLoop();
			String failMessage = "A for loop was found.";
			assertEquals(false, foundForLoop, failMessage);
			failMessage = "A while loop was found.";
			assertEquals(false, foundWhileLoop, failMessage);
			failMessage = "A for each loop was found.";
			assertEquals(false, foundForEachLoop, failMessage);
		} catch (Exception e) {
			System.out.println(e.getMessage());
			fail("Exceptopn occured.");
		}
	}

	/**
	 * Creates test data for the MinInt array. The list contains the input array and
	 * the correct value.
	 *
	 * @return List of test instances.
	 */
	private List<Tuple<List<Integer>, Integer>> getMinValueTestData() {
		ArrayList<Tuple<List<Integer>, Integer>> minValueTestValues = new ArrayList<>();
		List<Integer> array = Arrays.asList(24, 52, 74, 9, 34, 23, 64, 34);
		minValueTestValues.add(new Tuple<List<Integer>, Integer>(array, 9));
		array = Arrays.asList(24, 52, 74, 34, 23, 64, 20);
		minValueTestValues.add(new Tuple<List<Integer>, Integer>(array, 20));
		array = Arrays.asList(1, 2, 3, 4, 5, 6, 7);
		minValueTestValues.add(new Tuple<List<Integer>, Integer>(array, 1));
		array = Arrays.asList(7, 6, 5, 4, 3, 2, 1);
		minValueTestValues.add(new Tuple<List<Integer>, Integer>(array, 1));
		array = Arrays.asList(10, 20, 30, 5, 28, 22, 9);
		minValueTestValues.add(new Tuple<List<Integer>, Integer>(array, 5));
		return minValueTestValues;
	}

	/**
	 * Creates test data for the minimum in tree exercise. The list contains the
	 * input tree and the correct value.
	 *
	 * @return List of test instances.
	 */
	private List<Tuple<Tree, Integer>> getMinTreeTestData() {
		ArrayList<Tuple<Tree, Integer>> minTreeTestValues = new ArrayList<>();
		Tree tree = new Tree(24, new Tree(45, null, new Tree(8, null, null)),
				new Tree(17, new Tree(74, null, null), null));
		minTreeTestValues.add(new Tuple<Tree, Integer>(tree, 8));
		tree = new Tree(1, null, null);
		minTreeTestValues.add(new Tuple<Tree, Integer>(tree, 1));
		tree = new Tree(24, new Tree(45, new Tree(55, null, null), new Tree(73, null, null)),
				new Tree(17, new Tree(74, null, null), new Tree(34, null, null)));
		minTreeTestValues.add(new Tuple<Tree, Integer>(tree, 17));
		tree = new Tree(9, new Tree(8, new Tree(6, null, new Tree(4, null, null)), null),
				new Tree(7, null, new Tree(5, new Tree(3, null, null), null)));
		minTreeTestValues.add(new Tuple<Tree, Integer>(tree, 3));
		tree = new Tree(34, null, new Tree(56, null, new Tree(34, null, new Tree(78, new Tree(11, null, null),
				new Tree(23, null, new Tree(12, null, new Tree(67, null, new Tree(93, null, null))))))));
		minTreeTestValues.add(new Tuple<Tree, Integer>(tree, 11));
		return minTreeTestValues;
	}

}