import uk.ac.soton.ecs.comp1206.labtestlibrary.interfaces.io.*;
import java.io.*;
import java.lang.IllegalArgumentException;

public class JavaFileUtil implements ConcatenateJavaFiles {
  public void concatenateJavaFiles(String dirName, String fileName)
      throws IllegalArgumentException {
    try {
      File dir = new File(dirName);
      if (dir.isDirectory() == false) {
        System.out.println(dirName);
        throw new IllegalArgumentException("First argument is not a directory!");
      }

      PrintWriter out = new PrintWriter(dirName + File.separator + fileName);
      String[] fileNames = dir.list();

      for (String file : fileNames) {
        if (file.contains(".java")) {
          File f = new File(dir, file);

          BufferedReader br = new BufferedReader(new FileReader(f));

          String line = br.readLine();
          while (line != null) {
            out.println(line);
            line = br.readLine();
          }
          out.flush();
				}
      }

    } catch (Exception e) {
      System.out.println(e);
      throw new IllegalArgumentException("First argument is not a directory!");
    }

    return;
  }
}
