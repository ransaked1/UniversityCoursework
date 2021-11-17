import java.util.Locale;

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
}
