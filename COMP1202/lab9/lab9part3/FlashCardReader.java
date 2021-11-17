import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;

public class FlashCardReader {

  BufferedReader reader;
  ArrayList<FlashCard> cardList = new ArrayList<FlashCard>();

  /**
   * Flash card constructor that takes a filename and makes a buffer reader with it.
   *
   * @param filename Takes the file name to make a reader for.
   */
  public FlashCardReader(String filename) {
    try {
      reader = new BufferedReader(new FileReader(filename));
    } catch (FileNotFoundException e) {
      System.out.println(e);
    }
  }

  /**
   * Check that the file can be read.
   *
   * @return Return true if ready.
   */
  public boolean fileIsReady() {
    try {
      if (reader != null) {
        return reader.ready();
      }
    } catch (IOException e) {
      System.out.println(e);
    }
    return false;
  }

  /**
   * Get the flash card into an array list.
   *
   * @return Return the array list.
   */
  public ArrayList<FlashCard> getFlashCards() {
    while (true) {
      String tmp = this.getLine();
      if (tmp == null) {
        break;
      }
      int index = tmp.indexOf(":");

      String question = tmp.substring(0, index);
      String answer = tmp.substring(index + 1, tmp.length());

      FlashCard card = new FlashCard(question, answer);
      cardList.add(card);
    }
    return cardList;
  }

  /**
   * Get line from buffer.
   *
   * @return Return the line as a string.
   */
  public String getLine() {
    try {
      return reader.readLine();
    } catch (IOException e) {
      System.out.println(e);
    }
    return "ERROR";
  }
}
