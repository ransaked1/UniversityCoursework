import java.util.HashMap;
import java.util.HashSet;
import java.util.Locale;
import java.util.Set;

/**
 * Word group object that makes a sentence lower case.
 */
public class WordGroup {
  String words;

  public WordGroup(String wordsIn) {
    words = wordsIn.toLowerCase();
  }

  /**
   * Split the sentence string into words.
   *
   * @return String array with all the words.
   */
  public String[] getWordArray() {
    return words.split("\\s+");
  }

  /**
   * Takes a wordgroup and makes a hashset of words it contains in its sentence.
   *
   * @param wg Word group object to operate on.
   * @return Return the hashset with the words.
   */
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

  /**
   * Takes a wordgroup and makes a hashmap of words it contains in its sentence.
   *
   * @return Return the map with the  of the word group.
   */
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
