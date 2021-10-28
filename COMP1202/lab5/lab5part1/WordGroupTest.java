import static org.junit.jupiter.api.Assertions.*;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.ArrayList;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class WordGroupTest {

	class OutputCapturer {
		private PrintStream origOut;

		private ByteArrayOutputStream outputStream;

		public void start()
		{
			this.origOut = System.out;
			this.outputStream = new ByteArrayOutputStream();
			PrintStream ps = new PrintStream(this.outputStream);
			System.setOut(ps);
		}

		public String getOutput() {
			System.out.flush();
			return this.outputStream.toString().replaceAll("\\r\\n", "\n").replaceAll("\\r", "\n");
		}
		public void stop() {
			System.setOut(this.origOut);
		}
	}

	private static String platoString = "You can discover more about a person in an hour of play than in a year of conversation";
	private static String[] expectedPlato = new String[] {"you","can","discover","more","about","a","person","in","an","hour","of","play","than","in","a","year","of","conversation"};

	private static String rooseveltString = "When you play play hard when you work dont play at all";
	private static String[] expectedRoosevelt = new String[] {"when", "you", "play", "play", "hard", "when", "you", "work", "dont", "play", "at", "all"};

	@Test
	@DisplayName("Test WordGroup is created")
	void testWordGroup() {
		WordGroup sut = new WordGroup(platoString);
		assertNotNull(sut, "checks WordGroup is not null");
	}

	@Test
	@DisplayName("Test the WordGroup contents for plato")
	void testPlato() {
		WordGroup sut = new WordGroup(platoString);
		assertArrayEquals(expectedPlato, sut.getWordArray(), "checks that getWordArray returns an Array");
	}

	@Test
	@DisplayName("Test the WordGroup contents for roosevelt")
	void testRoosevelt() {
		WordGroup sut = new WordGroup(rooseveltString);
		assertArrayEquals(expectedRoosevelt, sut.getWordArray(), "checks that getWordArray returns an Array");
	}

	@Test
	@DisplayName("Test the WordGroup contents for shakespeare")
	void testShakespeare() {
		String testString = "The better part of valor is discretion";
		String []expected = new String[] {"the", "better", "part", "of", "valor", "is", "discretion"};

		WordGroup sut = new WordGroup(testString);
		assertArrayEquals(expected, sut.getWordArray(), "checks that getWordArray returns an Array");
	}

	@Test
	@DisplayName("Test words field")
	void testField() {
		try {
			Field words = WordGroup.class.getDeclaredField("words");
			assertNotNull(words, "tests there is a word field");
		} catch (NoSuchFieldException e) {
			fail("Missing field: words");
		}
	}

	@Test
	@DisplayName("Test main output")
	public void testMain(){

		String[] args = null;
		OutputCapturer outputHarness = new OutputCapturer();
		outputHarness.start();

		Main.main(args);

		outputHarness.stop();

		String output = outputHarness.getOutput();

		String jP = String.join("\n", expectedPlato);
		String jR = String.join("\n", expectedRoosevelt);

		assertTrue(output.contains(jP), "contains the correct string");
		assertTrue(output.contains(jR), "contains the correct string");

	}
}
