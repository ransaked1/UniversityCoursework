import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

/**
 * Word group object that makes a sentence lower case.
 */
public class Main {

  /**
   * Creating a word group object with sample data and print it word by word. Then do it with a
   * hashset and hashmap.
   *
   * @param args Standard terminal input.
   */
  public static void main(String[] args) {
    WordGroup wg1 =
        new WordGroup(
            "You can discover more about a person in an hour "
                + "of play than in a year of conversation");
    WordGroup wg2 = new WordGroup("When you play play hard when you work dont play at all");
    String[] s1 = wg1.getWordArray();
    String[] s2 = wg2.getWordArray();

    HashSet<String> hashs = wg1.getWordSet(wg2);
    HashMap<String, Integer> hashm1 = wg1.getWordCounts();
    HashMap<String, Integer> hashm2 = wg2.getWordCounts();

    Set<String> keySet1 = hashm1.keySet();
    for (String word : keySet1) {
      System.out.println(word + ": " + hashm1.get(word));
    }

    Set<String> keySet2 = hashm2.keySet();
    for (String word : keySet2) {
      System.out.println(word + ": " + hashm2.get(word));
    }

    for (String word : hashs) {
      if (hashm1.get(word) != null) {
        System.out.println(word + ": " + hashm1.get(word));
      }
    }

    for (String word : hashs) {
      if (hashm2.get(word) != null) {
        System.out.println(word + ": " + hashm2.get(word));
      }
    }
  }
}
