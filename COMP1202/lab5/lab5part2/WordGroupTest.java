import static org.junit.jupiter.api.Assertions.*;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;

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
	private static String rooseveltString = "When you play play hard when you work dont play at all";
	private static HashSet<String> words = new HashSet<>(Arrays.asList("play","all","discover","a","in","year","more","work","about","an","when","can","at","hour","person","of","than","hard","you","conversation","dont"));
	private static String w = "play all discover a in year more work about an when can at hour person of than hard you conversation dont";


	@Test
	@DisplayName("Test word set")
	void testGetWordSet() {

		WordGroup plato = new WordGroup(platoString);
		WordGroup roosevelt = new WordGroup(rooseveltString);
		HashSet<String> allWords = plato.getWordSet(roosevelt);

		assertTrue(plato.getWordSet(roosevelt) instanceof java.util.HashSet, "Check that roosevelt is of type HashSet");
		assertTrue(words.equals(allWords), "Check that getWordSet returns the correct values");
	}

	public int numOccurencies(String needle, String haystack) {
		if (needle.length() == 0) {
			return 0;
		}
		String reduced = haystack.replace(needle, "");
		int diff = haystack.length() - reduced.length();

		return diff / needle.length();
	}

	@Test
	@DisplayName("Test word group main")
	public void testMain(){
		OutputCapturer outputHarness = new OutputCapturer();
		outputHarness.start();

		String[] args = null;
		Main.main(args);

		outputHarness.stop();

		String output = outputHarness.getOutput();

		assertTrue((numOccurencies("play", output)==1) || (numOccurencies("play", output)==5), "test play was output the correct number of times");
		assertTrue((numOccurencies("you", output)==1) || (numOccurencies("you", output)==4), "test you was output the correct number of times");
		assertTrue((numOccurencies("conversation", output)==1) || (numOccurencies("conversation", output)==2), "test conversation was output the correct number of times");
	}

}
