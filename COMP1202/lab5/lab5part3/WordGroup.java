import java.util.Locale;
import java.util.HashSet;
import java.util.HashMap;
import java.util.Set;

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

  public HashMap<String, Integer> getWordCounts() {
    HashMap<String, Integer> map = new HashMap<String, Integer>();

    for (String word : this.getWordArray()) {
      if (map.get(word) != null) {
        map.put(word, map.get(word) + 1);
      } else {
        map.put(word, 1);
      }
    }

    return map;
  }
}
