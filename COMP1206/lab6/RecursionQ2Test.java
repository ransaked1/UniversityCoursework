import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.File;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.junit.jupiter.api.Test;

import uk.ac.soton.ecs.comp1206.labtestlibrary.io.FileSystemAccess;
import uk.ac.soton.ecs.comp1206.labtestlibrary.parse.JavaFile;
import uk.ac.soton.ecs.comp1206.labtestlibrary.recursion.RecursionChecker;
import uk.ac.soton.ecs.comp1206.labtestlibrary.recursion.Tuple;

/**
 *
 * Tests for the Lab of recursion.
 * Question 2: kPalindrome.
 *
 * @author jan
 *
 */
class RecursionQ2Test {

	/**
	 * Test that the correct value is returned for the exercise to recursively check
	 * if a word is a k-palindrome.
	 * All instances are k-Palindromes.
	 */
	@Test
	void isKPalindromeCorrectValueYesInstancesTest() {
		List<Tuple<Tuple<String, Integer>, Boolean>> testData = getKPalindromeTestDataYesInstances();
		for (Tuple<Tuple<String, Integer>, Boolean> testDatum : testData) {
			String testWord = testDatum.getFirstValue().getFirstValue();
			int testK = testDatum.getFirstValue().getSecondValue().intValue();
			boolean expected = testDatum.getSecondValue();
			KPalindrome kPalindrome = new KPalindrome();
			boolean result = kPalindrome.isKPalindrome(testWord, testK);
			String failMessage = "Wrong k-palindrome result for word: " + testWord + ", is a k-Palindromes for k: " + testK + ".";
			assertEquals(expected, result, failMessage);
		}
	}

	/**
	 * Test that the correct value is returned for the exercise to recursively check
	 * if a word is a k-palindrome.
	 * All instances are not k-Palindromes.
	 */
	@Test
	void isKPalindromeCorrectValueNoInstancesTest() {
		List<Tuple<Tuple<String, Integer>, Boolean>> testData = getKPalindromeTestDataNoInstances();
		for (Tuple<Tuple<String, Integer>, Boolean> testDatum : testData) {
			String testWord = testDatum.getFirstValue().getFirstValue();
			int testK = testDatum.getFirstValue().getSecondValue().intValue();
			boolean expected = testDatum.getSecondValue();
			KPalindrome kPalindrome = new KPalindrome();
			boolean result = kPalindrome.isKPalindrome(testWord, testK);
			String failMessage = "Wrong k-palindrome result for word: " + testWord + ", is NOT a k-Palindromes for k: " + testK + ".";
			assertEquals(expected, result, failMessage);
		}
	}

	/**
	 * Test that no loops where used in the exercise to recursively check if a word
	 * is a k-palindrome.
	 */
	@Test
	void kPalindromeNoLoopsTest() {
		checkIfBadStatementsWhereUsed("KPalindrome");
	}

	/**
	 * Test a class file if it contains any types of loops.
	 * The file is searched for based on the class name.
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
	 * Creates a set of k-Palindrome test that are k-Palindromes.
	 *
	 * @return A list of pairs of k-Palindrome instances and the result.
	 */
	private List<Tuple<Tuple<String, Integer>, Boolean>> getKPalindromeTestDataYesInstances() {
		ArrayList<Tuple<Tuple<String, Integer>, Boolean>> kPalindromeTestValues = new ArrayList<>();
		kPalindromeTestValues
				.add(new Tuple<Tuple<String, Integer>, Boolean>(new Tuple<String, Integer>("CABCBC", 2), true));
		kPalindromeTestValues
				.add(new Tuple<Tuple<String, Integer>, Boolean>(new Tuple<String, Integer>("CABCBC", 1), true));
		kPalindromeTestValues
				.add(new Tuple<Tuple<String, Integer>, Boolean>(new Tuple<String, Integer>("ABCDEF", 6), true));
		kPalindromeTestValues
				.add(new Tuple<Tuple<String, Integer>, Boolean>(new Tuple<String, Integer>("ABCBA", 0), true));
		kPalindromeTestValues
				.add(new Tuple<Tuple<String, Integer>, Boolean>(new Tuple<String, Integer>("XDBGBD", 1), true));
		kPalindromeTestValues
				.add(new Tuple<Tuple<String, Integer>, Boolean>(new Tuple<String, Integer>("XDBGBDY", 2), true));
		kPalindromeTestValues
				.add(new Tuple<Tuple<String, Integer>, Boolean>(new Tuple<String, Integer>("XDBZGBDY", 3), true));
		kPalindromeTestValues
				.add(new Tuple<Tuple<String, Integer>, Boolean>(new Tuple<String, Integer>("XDBZGWBDY", 4), true));
		return kPalindromeTestValues;
	}

	/**
	 * Creates a set of k-Palindrome test that are not k-Palindromes.
	 *
	 * @return A list of pairs of k-Palindrome instances and the result.
	 */
	private List<Tuple<Tuple<String, Integer>, Boolean>> getKPalindromeTestDataNoInstances() {
		ArrayList<Tuple<Tuple<String, Integer>, Boolean>> kPalindromeTestValues = new ArrayList<>();
		kPalindromeTestValues
				.add(new Tuple<Tuple<String, Integer>, Boolean>(new Tuple<String, Integer>("CABCBCYZ", 2), false));
		kPalindromeTestValues
				.add(new Tuple<Tuple<String, Integer>, Boolean>(new Tuple<String, Integer>("ABCDEF", 3), false));
		kPalindromeTestValues
				.add(new Tuple<Tuple<String, Integer>, Boolean>(new Tuple<String, Integer>("ABCBAZ", 0), false));
		kPalindromeTestValues
				.add(new Tuple<Tuple<String, Integer>, Boolean>(new Tuple<String, Integer>("YXDBGBD", 1), false));
		kPalindromeTestValues
				.add(new Tuple<Tuple<String, Integer>, Boolean>(new Tuple<String, Integer>("XDBZGBDY", 2), false));
		kPalindromeTestValues
				.add(new Tuple<Tuple<String, Integer>, Boolean>(new Tuple<String, Integer>("XDLLBZRGPBDMY", 3), false));
		kPalindromeTestValues
				.add(new Tuple<Tuple<String, Integer>, Boolean>(new Tuple<String, Integer>("LXDBTTZGWBDKY", 4), false));
		return kPalindromeTestValues;
	}
}