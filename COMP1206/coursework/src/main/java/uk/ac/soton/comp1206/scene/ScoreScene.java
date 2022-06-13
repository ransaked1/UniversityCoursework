package uk.ac.soton.comp1206.scene;

import java.util.Iterator;
import java.util.Timer;
import java.util.TimerTask;
import javafx.scene.control.Button;
import javafx.scene.control.TextField;
import javafx.util.Pair;

import uk.ac.soton.comp1206.game.Game;
import uk.ac.soton.comp1206.network.Communicator;
import uk.ac.soton.comp1206.scene.build.ScoreSceneBuilder;
import uk.ac.soton.comp1206.ui.GameWindow;
import uk.ac.soton.comp1206.util.Constants;
import uk.ac.soton.comp1206.util.Multimedia;
import uk.ac.soton.comp1206.util.Storage;

import static javafx.application.Platform.*;

/**
 * The Score Scene. Requests the remote scores and updates both the remote and local scores with the
 * new high score.
 */
public class ScoreScene extends ScoreSceneBuilder {

  /** Timeout timer for scene. */
  private Timer timer;

  /** Booleans for new high scores. */
  private boolean newLocalScore = false;
  private boolean newRemoteScore = false;

  /** Server response variable */
  private boolean waitingForScores = true;

  /**
   * Constructor for the score scene after a game is over.
   *
   * @param gameWindow the Game Window
   * @param game the game finished before the scene
   */
  public ScoreScene(GameWindow gameWindow, Game game) {
    super(gameWindow, game);
  }

  /**
   * Constructor for the leaderboard scene.
   *
   * <p>Doesn't need a game because of the dummy used.
   *
   * @param gameWindow the Game Window
   */
  public ScoreScene(GameWindow gameWindow) {
    super(gameWindow);
  }

  /** Initialize the Score Scene */
  @Override
  public void initialise() {
    playGameEndMusic();

    if (!game.getScores().isEmpty()) {
      localName.set(game.nameProperty().getValue());
    }

    // Check is the app has access to the server and show online score if yes.
    if (Communicator.isOnline()) {
      communicator.addListener((message) -> runLater(() -> receiveMessage(message.trim())));
      communicator.send("HISCORES");
    } else {
      waitingForScores = false;
      checkForHighScore();
    }
  }

  /** Play end music when the game is over (Overwritten in Leaderboard scene). */
  protected void playGameEndMusic() {
    Multimedia.startBackgroundMusic("end.wav",false);
  }

  /** Stop any other timers and schedule the scene to return to the main menu. */
  private void startTimer() {
    if (timer != null) {
      timer.cancel();
      timer.purge();
    }

    var task =
        new TimerTask() {
          public void run() {
            runLater(ScoreScene.this::returnToMenu);
          }
        };

    timer = new Timer();
    timer.schedule(task, Constants.WAIT_TIME);
  }

  /** Return to the main menu */
  protected void returnToMenu() {
    if (newLocalScore) {
      return;
    }

    if (timer != null) {
      timer.cancel();
    }

    gameWindow.startMenu();
  }

  /** Check if there are any high scores to add to the list of scores. */
  public void checkForHighScore() {
    // logger.info("Checking new scores");
    if (game.getScores().isEmpty()) {
      int currentScore = game.getScore();
      int localCounter = 0;
      int remoteCounter = 0;

      int lowestScoreLocal = checkLocalScoreAddition();
      int lowestScoreRemote = checkRemoteScoreAddition();

      // Find position of the new local high score
      localCounter = checkPositionLocal(currentScore, localCounter, lowestScoreLocal);

      // Find position of the new online high score if there is online access
      if (Communicator.isOnline()) {
        remoteCounter = checkPositionRemote(currentScore, remoteCounter, lowestScoreRemote);
      }

      // If a new high score is added ask for the name of the player
      if (newLocalScore || newRemoteScore) {

        // Generating UI elements
        TextField name = generateTextField();
        Button button = generateSubmitButton();

        int finalCounter = localCounter;
        int finalRemoteCounter = remoteCounter;

        // Submit button action
        button.setOnAction(
            (e) -> submitRemote(currentScore, name, finalCounter, finalRemoteCounter));
      } else {
        reveal();
      }
    } else {
      reveal();
    }
  }

  /**
   * Get the lowest online high score.
   *
   * @return the lowest online score value
   */
  private int checkRemoteScoreAddition() {
    int lowestScoreRemote = 0;
    if (remoteScoresList.size() > 0) {
      lowestScoreRemote = (remoteScoresList.get(remoteScoresList.size() - 1)).getValue();
    }

    if (remoteScoresList.size() < Constants.TOP_SCORE_COUNT && Communicator.isOnline()) {
      newRemoteScore = true;
    }
    return lowestScoreRemote;
  }

  /**
   * Get the lowest local high score.
   *
   * @return the lowest local score value
   */
  private int checkLocalScoreAddition() {
    int lowestScoreLocal = 0;

    if (localScores.size() > 0) {
      lowestScoreLocal = (localScores.get(localScores.size() - 1)).getValue();
    }

    if (localScores.size() < Constants.TOP_SCORE_COUNT) {
      newLocalScore = true;
    }
    return lowestScoreLocal;
  }

  /**
   * Generate submit button.
   *
   * @return the button UI element
   */
  private Button generateSubmitButton() {
    Button button = new Button("Submit");
    button.setDefaultButton(true);
    scoreBox.getChildren().add(3, button);
    return button;
  }

  /**
   * Generate text field for name input.
   *
   * @return the text field UI element
   */
  private TextField generateTextField() {
    var name = new TextField();
    name.setPromptText("Enter your name");
    name.setPrefWidth(gameWindow.getWidth() / 4.0);
    name.setMaxWidth(gameWindow.getWidth() / 3.0);
    name.requestFocus();
    scoreBox.getChildren().add(2, name);
    return name;
  }

  /**
   * Find position of the new high score to be added to remote.
   *
   * @param currentScore player score at the end of the game
   * @param counter the position counter
   * @param lowestScoreRemote the lowest score at remote
   * @return the position of the new high score
   */
  private int checkPositionRemote(int currentScore, int counter, int lowestScoreRemote) {
    Iterator<Pair<String, Integer>> iterator;
    Pair<String, Integer> score;

    if (currentScore > lowestScoreRemote) {
      for (iterator = remoteScoresList.iterator(); iterator.hasNext(); ++counter) {
        score = iterator.next();
        if (score.getValue() < currentScore) {
          // logger.info("New remote high score at {}", counter);
          newRemoteScore = true;
          break;
        }
      }
    }
    return counter;
  }

  /**
   * Find position of the new high score to be added to local.
   *
   * @param currentScore player score at the end of the game
   * @param counter the position counter
   * @param lowestScoreLocal the lowest score at local
   * @return the position of the new high score
   */
  private int checkPositionLocal(int currentScore, int counter, int lowestScoreLocal) {
    Iterator<Pair<String, Integer>> iterator;
    Pair<String, Integer> score;

    if (currentScore > lowestScoreLocal) {
      for (iterator = this.localScores.iterator(); iterator.hasNext(); ++counter) {
        score = iterator.next();
        if (score.getValue() < currentScore) {
          // logger.info("New local high score at {}", counter);
          newLocalScore = true;
          break;
        }
      }
    }
    return counter;
  }

  /**
   * Submit the new high score to the local and remote.
   *
   * @param currentScore the score to be added
   * @param name the name of the player that has the score
   * @param finalLocalCounter the position in the local score list
   * @param finalRemoteCounter the position in the remote score list
   */
  private void submitRemote(
      int currentScore, TextField name, int finalLocalCounter, int finalRemoteCounter) {
    // Get the name of the local player
    String localName = name.getText().replace(":", "");
    localName = localName.substring(0, Math.min(localName.length(), Constants.TOP_SCORE_COUNT));
    this.localName.set(localName);

    scoreBox.getChildren().remove(2);
    scoreBox.getChildren().remove(2);
    Pair<String, Integer> localScore = new Pair<>(localName, currentScore);

    // Update the local score list
    if (newLocalScore) {
      localScores.add(finalLocalCounter, localScore);
    }

    // Update the remote score list
    if (newRemoteScore) {
      remoteScores.add(finalRemoteCounter, localScore);
    }

    // Write local score list to storage
    Storage.writeScores(localScores);

    // If available write score to remote
    if (Communicator.isOnline()) {
      communicator.send("HISCORE " + localName + ":" + currentScore);
      communicator.send("HISCORES");
    } else {
      reveal();
    }

    // Reset variables
    newLocalScore = false;
    newRemoteScore = false;

    Multimedia.playAudio("highscore.wav");
  }

  /** Reveal the two score lists and return to the menu */
  protected void reveal() {
    startTimer();
    scene.setOnKeyPressed((e) -> returnToMenu());
    showScores.set(true);
    highScoreColumn1.reveal();
    highScoreColumn2.reveal();
  }

  /**
   * Get message from the server.
   *
   * @param message Raw message from the server
   */
  private void receiveMessage(String message) {
    String[] components = message.split(" ", 2);
    String command = components[0];

    if (command.equals("HISCORES")) {
      if (components.length > 1) {
        String data = components[1];
        receiveScores(data);
      } else {
        receiveScores("");
      }
    }
  }

  /**
   * Receive the scores from the server and parse the data.
   *
   * <p>Add the list to the UI.
   *
   * @param data high scores data
   */
  private void receiveScores(String data) {
    remoteScoresList.clear();
    String[] scoreLines = data.split("\\R");

    // Parse high score data
    for (String scoreLine : scoreLines) {
      String[] components = scoreLine.split(":", 2);
      String player = components[0];
      int score = Integer.parseInt(components[1]);
      remoteScoresList.add(new Pair<>(player, score));
    }

    // Sort the received list
    remoteScoresList.sort((a, b) -> b.getValue().compareTo(a.getValue()));
    remoteScores.clear();
    remoteScores.addAll(remoteScoresList);

    // Check if there is a new high score and reveal the list
    if (waitingForScores) {
      checkForHighScore();
      waitingForScores = false;
    } else {
      reveal();
    }
  }
}
