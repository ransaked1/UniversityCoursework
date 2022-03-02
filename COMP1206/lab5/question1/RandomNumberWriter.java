import uk.ac.soton.ecs.comp1206.labtestlibrary.interfaces.io.*;
import java.util.Random;
import java.io.*;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;

public class RandomNumberWriter implements RandomIO {

  private Random rand;

  public RandomNumberWriter(long seed) {
    rand = new Random();
    rand.setSeed(seed);
  }

  public void writeRandomChars(String path) {

    File f;
    Writer out;

    try {
      f = new File(path);
      out = new FileWriter(f);

      for (int i = 0; i < 10000; i++) {
        Integer number = Integer.valueOf(rand.nextInt(100000));
        out.write(number.toString() + '\n');
      }

      out.close();
    } catch (Exception e) {
      System.out.println(e);
    }

    return;
  }

  public void writeRandomByte(String path) {

    File f;
    OutputStream outStream;

    try {
      f = new File(path);
      outStream = new FileOutputStream(f);

      for (int i = 0; i < 10000; i++) {
        Integer number = Integer.valueOf(rand.nextInt(100000));
        byte b[] = ByteBuffer.allocate(4).putInt(number).array();
        outStream.write(b);
      }

      outStream.close();
    } catch (Exception e) {
      System.out.println(e);
    }

    return;
  }
}
