import java.util.Locale;

public class WordGroup {
  String words;

  public WordGroup(String wordsIn) {
    words = wordsIn.toLowerCase();
  }

  public String[] getWordArray() {
    return words.split("\\s+");
  }
}
