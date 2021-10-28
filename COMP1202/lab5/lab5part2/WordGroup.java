import java.util.Locale;
import java.util.HashSet;

public class WordGroup {
	String words;

	public WordGroup(String wordsIn) {
		words = wordsIn.toLowerCase();
	}

	public String[] getWordArray() {
		return words.split("\\s+");
	}

	public HashSet<String> getWordSet(WordGroup wg) {
		HashSet<String> stringHashSet = new HashSet<String>();

		for (String word : this.getWordArray()) {
			stringHashSet.add(word);
		}

		for (String word : wg.getWordArray()) {
			stringHashSet.add(word);
		}

		return stringHashSet;
	}
}