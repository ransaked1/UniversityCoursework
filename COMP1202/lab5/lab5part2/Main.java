import java.util.HashSet;

public class Main {
  public static void main(String[] args) {
    WordGroup wg1 =
        new WordGroup(
            "You can discover more about a person in an hour of play than in a year of conversation");
    WordGroup wg2 = new WordGroup("When you play play hard when you work dont play at all");
    String[] s1 = wg1.getWordArray();
    String[] s2 = wg2.getWordArray();

    for (String word : s1) {
      System.out.println(word);
    }

    for (String word : s2) {
      System.out.println(word);
    }

    HashSet<String> hashs = wg1.getWordSet(wg2);
    for (String word : hashs) {
      System.out.println(word);
    }
  }
}
