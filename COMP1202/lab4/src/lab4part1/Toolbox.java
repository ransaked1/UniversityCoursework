package lab4part1;

import java.io.*;
import java.lang.Integer;
import java.util.ArrayDeque;
import java.util.Arrays;
import java.util.Random;

/**
 * Provides convenience methods for I/O and other basic operations.
 * Only one instance of the Toolbox class should be created.
 * Sample data can be provided to simulate user input for testing.
 * This class is not thread safe.
 */
public class Toolbox {

	/**
	 * A value indicating whether {@link Toolbox} instances should consume test values rather than prompting the user for input or querying a random number generator.
	 */
	private static Boolean testing = false;

	/**
	 * The remaining integer values to be returned by {@link #getRandomInteger(Integer)} instead of pseudo-random numbers.
	 * <code>null</code> When {@link #testing} is <code>false</code>
	 */
	private static ArrayDeque<Integer> randomTestValues = null;

	/**
	 * The remaining strings to be returned by {@link #readStringFromCmd()} instead of user input.
	 * <code>null</code> When {@link #testing} is <code>false</code>
	 */
	private static ArrayDeque<String> inputTestValues = null;

	/**
	 * The random number generator for the {@link Toolbox} instance.
	 */
	private Random rand = new Random();

	/**
	 * Sets the testing data to be used by all {@link Toolbox} instances.
	 * The values will be consumed by Toolbox instances instead of prompting the user for input or querying a random number generator.
	 * @param randomValues	The integer values to be returned by {@link #getRandomInteger(Integer)} instead of pseudo random numbers.
	 * @param inputValues	The strings to be returned by {@link #readStringFromCmd()} instead of user input.
	 */
	public static void setTestingData(Integer[] randomValues, String[] inputValues) {
		testing = true;
		Toolbox.randomTestValues = new ArrayDeque<Integer>(Arrays.asList(randomValues));
		Toolbox.inputTestValues = new ArrayDeque<String>(Arrays.asList(inputValues));
	}

	/**
	 * Gets a random integer in the range [1, max] inclusive.
	 * @param max	The inclusive upper bound for the random number. Must be no less than 1.
	 */
	public Integer getRandomInteger(Integer max) {
		if (max < 1) {
			throw new IllegalArgumentException("Max value must be greater than zero.");
		}

		if (Toolbox.testing) {
			Integer number = Toolbox.randomTestValues.poll();

			if (number == null) {
				throw new Error("Ran out of random integers from the test data. This may indicate that getRandomInteger was called too many times.");
			}

			int value = number.intValue();
			if (value < 1 || value > max) {
				throw new Error("Integer from test data was outside the expected range. This may indicate that getRandomInteger was called with an inappropriate range.");
			}

			return number;
		}
		else {
			int value = rand.nextInt(max) + 1;
			Integer number = Integer.valueOf(value);
			return number;
		}
	}

	/**
	 * Prompts the user for input, and reads an integer from the command line.
	 * In the case of an invalid input or an I/O error, a message will be printed to the user, and <code>null</code> returned from the method.
	 */
	public Integer readIntegerFromCmd() {
		System.out.println("Enter your number:");

		try {
			String input = readStringInternal();
			Integer number = Integer.valueOf(input);
			return number;
		} catch (NumberFormatException e) {
			if (Toolbox.testing) {
				throw new Error("Test input was not an integer. This may indicate that readIntegerFromCmd or readStringFromCmd was called the wrong number of times.");
			} else {
				System.err.println("There is something wrong with the number you entered.");
				return null;
			}
		}
	}

	/**
	 * Prompts the user for input, and reads a line of input from the command line.
	 * In the case of an I/O error, a message will be printed to the user, and <code>null</code> returned from the method.
	 */
	public String readStringFromCmd() {
		System.out.println("Enter your string:");
		return readStringInternal();
	}

	/**
	 * Reads a line of input from the command line.
	 * In the case of an I/O error, a message will be printed to the user, and <code>null</code> returned from the method.
	 */
	private String readStringInternal() {
		if (Toolbox.testing) {
			String testInput = Toolbox.inputTestValues.poll();

			if (testInput == null) {
				throw new Error("Ran out of inputs from the test data. This may indicate that readIntegerFromCmd or readStringFromCmd was called too many times.");
			}

			return testInput;
		} else {
			BufferedReader br = new BufferedReader(new InputStreamReader(System.in));

			try {
				String input = br.readLine();
				return input;
			} catch (IOException ioe) {
				System.err.println("There was an input error.");
				return null;
			}
		}
	}
}
