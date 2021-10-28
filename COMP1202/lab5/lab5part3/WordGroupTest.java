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
	@DisplayName("Test the word count method with plato")
	void testGetWordCountPlato() {
		WordGroup plato = new WordGroup(platoString);
		HashMap<String, Integer> wordCounts = plato.getWordCounts();

		assertTrue(plato.getWordCounts() instanceof java.util.HashMap, "Check that getWordCounts returns is a HashMap");
		assertTrue(wordCounts.get("play").equals(1), "play = 1");
		assertTrue(wordCounts.get("discover").equals(1), "discover = 1");
		assertTrue(wordCounts.get("a").equals(2), "a = 2");
		assertTrue(wordCounts.get("in").equals(2), "in = 2");
		assertTrue(wordCounts.get("year").equals(1), "year = 1");
		assertTrue(wordCounts.get("more").equals(1), "more = 1");
		assertTrue(wordCounts.get("about").equals(1), "about = 1");
		assertTrue(wordCounts.get("an").equals(1), "an = 1");
		assertTrue(wordCounts.get("can").equals(1), "can = 1");
		assertTrue(wordCounts.get("hour").equals(1), "hour = 1");
		assertTrue(wordCounts.get("person").equals(1), "person = 1");
		assertTrue(wordCounts.get("of").equals(2), "of = 2");
		assertTrue(wordCounts.get("than").equals(1), "than = 1");
		assertTrue(wordCounts.get("you").equals(1), "you = 1");
		assertTrue(wordCounts.get("conversation").equals(1), "conversation = 1");

	}

	@Test
	@DisplayName("Test the word count method with roosevelt")
	void testGetWordCountRoosevelt() {
		WordGroup roosevelt = new WordGroup(rooseveltString);
		HashMap<String, Integer> wordCounts = roosevelt.getWordCounts();

		assertTrue(roosevelt.getWordCounts() instanceof java.util.HashMap, "Check that getWordCounts returns is a HashMap");
		assertTrue(wordCounts.get("play").equals(3), "play = 3");
		assertTrue(wordCounts.get("all").equals(1), "all = 1");
		assertTrue(wordCounts.get("at").equals(1), "at = 1");
		assertTrue(wordCounts.get("work").equals(1), "work = 1");
		assertTrue(wordCounts.get("hard").equals(1), "hard = 1");
		assertTrue(wordCounts.get("when").equals(2), "when = 2");
		assertTrue(wordCounts.get("you").equals(2), "you = 2");
		assertTrue(wordCounts.get("dont").equals(1), "dont = 1");

	}

	@Test
	@DisplayName("Test the word count method with tongue twister")
	void testGetWordCountsTwister() {
		WordGroup twister = new WordGroup("Red lorry yellow lorry red lorry");

		HashMap<String, Integer> wordCounts = twister.getWordCounts();

		assertTrue(twister.getWordCounts() instanceof java.util.HashMap, "Check that getWordCounts returns is a HashMap");
		assertTrue(wordCounts.get("red").equals(2), "red = 2");
		assertTrue(wordCounts.get("lorry").equals(3), "lorry = 3");
		assertTrue(wordCounts.get("yellow").equals(1), "yellow = 1");
	}

	@Test
	@DisplayName("Test word main")
	public void testMain(){
		OutputCapturer outputHarness = new OutputCapturer();
		outputHarness.start();

		String[] args = null;
		Main.main(args);

		outputHarness.stop();

		String output = outputHarness.getOutput();

		//Plato word counts
		assertTrue(output.contains("play: 1"), "Word count for play in plato should be 1");
		assertTrue(output.contains("discover: 1"), "Word count for discover in plato should be 1");
		assertTrue(output.contains("a: 2"), "Word count for a in plato should be 2");
		assertTrue(output.contains("in: 2"), "Word count for in in plato should be 2");
		assertTrue(output.contains("more: 1"), "Word count for more in plato should be 1");
		assertTrue(output.contains("about: 1"), "Word count for about in plato should be 1");
		assertTrue(output.contains("an: 1"), "Word count for an in plato should be 1");
		assertTrue(output.contains("can: 1"), "Word count for can in plato should be 1");
		assertTrue(output.contains("hour: 1"), "Word count for hour in plato should be 1");
		assertTrue(output.contains("person: 1"), "Word count for person in plato should be 1");
		assertTrue(output.contains("of: 2"), "Word count for of in plato should be 2");
		assertTrue(output.contains("than: 1"), "Word count for than in plato should be 1");
		assertTrue(output.contains("you: 1"), "Word count for you in plato should be 1");
		assertTrue(output.contains("conversation: 1"), "Word count for conversation in plato should be 1");
		//Roosevelt word counts
		assertTrue(output.contains("play: 3"), "Word count for play in roosevelt should be 3");
		assertTrue(output.contains("all: 1"), "Word count for all in roosevelt should be 1");
		assertTrue(output.contains("at: 1"), "Word count for at in roosevelt should be 1");
		assertTrue(output.contains("work: 1"), "Word count for work in roosevelt should be 1");
		assertTrue(output.contains("hard: 1"), "Word count for hard in roosevelt should be 1");
		assertTrue(output.contains("when: 2"), "Word count for when in roosevelt should be 2");
		assertTrue(output.contains("you: 2"), "Word count for you in roosevelt should be 2");
		assertTrue(output.contains("dont: 1"), "Word count for dont in roosevelt should be 1");
	}
}
