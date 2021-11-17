import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

public class FlashCardReader {

  BufferedReader reader;

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
}
