import bugs.*;
import building.*;
import students.*;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.regex.Pattern;
import java.util.regex.Matcher;

/**
 * Class that reads a config file and generates a bidimensional ArrayList with all the bug waves
 * from it.
 */
public class ConfigReader {
  // Final list that will contain all the bug waves
  ArrayList<ArrayList<Bug>> bugWaves = new ArrayList<ArrayList<Bug>>();

  /**
   * Constructor that takes a file name and reads it line by line putting the bug waves it parses
   * int othe final bugWaves variable.
   *
   * @param fileName The name of the config file.
   */
  public ConfigReader(String fileName) {
    try (BufferedReader br = new BufferedReader(new FileReader(fileName))) {
      String line;

      // Read a line from the file and split it at the semicolons into an array of strings
      // describing exctly one bug that are processed into an ArrayList representing a wave
      while ((line = br.readLine()) != null) {
        try {
          String[] bugStrings = line.split(";");
          bugWaves.add(processBugStrings(bugStrings));
        } catch (Exception e) {
          System.out.println();
          System.out.println("Game configuration file has incorrect format"); // File format error
          System.out.println();
          System.exit(0);
        }
      }
    } catch (IOException e) {
      System.out.println();
      System.err.println(e); // Throw error on failure to read from file
      System.out.println();
      System.exit(0);
    }
  }

  /**
   * Helper method that constructs an ArrayList out of an array of strings descirbing a bug each.
   *
   * @param bugStrings The strings to process.
   * @return The ArrayList of bugs of the wave.
   */
  private ArrayList<Bug> processBugStrings(String[] bugStrings) {
    ArrayList<Bug> bugWave = new ArrayList<Bug>(); // List of the bugs in the wave
    for (int i = 0; i < bugStrings.length; i++) {
      // Split the string in half at the round bracket openning and take the fisrt part which is the
      // bug name
      String[] splitAtBracket = bugStrings[i].split("\\(");
      String bugName = splitAtBracket[0];

      // Get the data inside the round brackets and split it at each comma
      Matcher matcher = Pattern.compile("\\((.*?)\\)").matcher(bugStrings[i]);
      matcher.find();
      String[] insideBrackets = matcher.group(1).split(",");

      // generate a bug with this parsed data and add it to the wave
      bugWave.add(processBugString(bugName, insideBrackets));
    }
    return bugWave;
  }

  /**
   * Helper function that generates a bug object from the data provided in string format.
   *
   * @param bugName The name of the bug to be created.
   * @param insideBrackets The data that is the brackets for that bug.
   * @return A bug object generated.
   */
  private Bug processBugString(String bugName, String[] insideBrackets) {
    // Converting and storing the data to the appropriate types
    String bugType = "";
    int bugLevel = 0;
    int bugSteps = 0;
    try {
      bugType = insideBrackets[0];
      bugLevel = Integer.valueOf(insideBrackets[1]);
      bugSteps = Integer.valueOf(insideBrackets[2]);
    } catch (Exception e) {
      System.out.println();
      System.out.println("Game configuration file has incorrect format"); // File format error
      System.out.println();
      System.exit(0);
    }

    // Identify bug type and create its object with the characteristics provided
    if (bugType.equals("CMB") || bugType.equals("ConcurrentModificationBug")) {
      return new ConcurrentModificationBug(bugName, bugLevel, bugSteps);
    } else if (bugType.equals("NPB") || bugType.equals("NullPointerBug")) {
      return new NullPointerBug(bugName, bugLevel, bugSteps);
    } else if (bugType.equals("NTB") || bugType.equals("NoneTerminationBug")) {
      return new NoneTerminationBug(bugName, bugLevel, bugSteps);
    } else {
      System.out.println();
      System.err.println("Unrecognized bug type in config file"); // Wrong bug name
      System.out.println();
      System.exit(0);
      return null;
    }
  }

  /**
   * Return the bug waves generated.
   *
   * @return bug waves bidimensional ArrayList
   */
  public ArrayList<ArrayList<Bug>> getBugWaves() {
    return bugWaves;
  }
}
