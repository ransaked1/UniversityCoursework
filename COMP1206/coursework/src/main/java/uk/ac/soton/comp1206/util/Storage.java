package uk.ac.soton.comp1206.util;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import javafx.util.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * Class that manages the local storage.
 */
public class Storage {

  private static final Logger logger = LogManager.getLogger(Storage.class);

  /**
   * Basic constructor.
   */
  public Storage() {
  }

  /**
   * Load scores from the scores.txt file.
   *
   * @return array of pairs of player name and their score
   */
  public static ArrayList<Pair<String, Integer>> loadScores() {
    logger.info("Loading local scores");
    ArrayList<Pair<String, Integer>> result = new ArrayList<>();

    try {
      var path = Paths.get("scores.txt");

      if (Files.notExists(path)) {
        initialiseScores();
      }

      List<String> scores = Files.readAllLines(path);
      for (String score : scores) {
        String[] components = score.split(":");
        result.add(new Pair<>(components[0], Integer.parseInt(components[1])));
      }
    } catch (Exception e) {
      e.printStackTrace();
    }

    return result;
  }

  /**
   * Initialize a dummy score.txt file.
   */
  public static void initialiseScores() {
    logger.info("Initialising scores file");
    ArrayList<Pair<String, Integer>> result = new ArrayList<>();

    for(int i = 0; i < Constants.TOP_SCORE_COUNT; ++i) {
      result.add(new Pair<>("Dummy", 0));
    }

    writeScores(result);
  }

  /**
   * Write scores to score.txt file.
   *
   * @param scores the scores to write
   */
  public static void writeScores(List<Pair<String, Integer>> scores) {
    logger.info("Writing {} scores to scores.txt", (long) scores.size());

    //Sort the score list before writing
    scores.sort((a, b) -> b.getValue().compareTo(a.getValue()));

    try {
      Path path = Paths.get("scores.txt");
      StringBuilder result = new StringBuilder();
      int counter = 0;

      for (Pair<String, Integer> score : scores) {
        counter++;
        String scoreString = score.getKey();
        result.append(scoreString).append(":").append(score.getValue()).append("\n");
        if (counter >= Constants.TOP_SCORE_COUNT) {
          break;
        }
      }
      Files.writeString(path, result.toString());
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}
