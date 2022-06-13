package uk.ac.soton.comp1206.scene.build;

import javafx.beans.property.*;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.text.Text;
import javafx.util.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import java.util.ArrayList;

import uk.ac.soton.comp1206.ui.ScoreField;
import uk.ac.soton.comp1206.network.Communicator;
import uk.ac.soton.comp1206.ui.GameWindow;

/** Builder for the Multiplayer Game Scene extending the challenge scene builder. */
public abstract class MultiplayerGameSceneBuilder extends ChallengeSceneBuilder {

  private static final Logger logger = LogManager.getLogger(MultiplayerGameSceneBuilder.class);

  /** Online game elements. */
  protected final Communicator communicator;
  protected ObservableList<Pair<String, Integer>> remoteScoreList;
  private final ArrayList<Pair<String, Integer>> remoteScores = new ArrayList<>();
  protected ScoreField leaderboard;
  private final StringProperty name = new SimpleStringProperty();

  /**
   * Create a new scene, passing in the GameWindow the scene will be displayed in.
   *
   * @param gameWindow the game window
   */
  public MultiplayerGameSceneBuilder(GameWindow gameWindow) {
    super(gameWindow);
    communicator = gameWindow.getCommunicator();
  }

  /** Build the Multiplayer Game window. */
  @Override
  public void build() {
    logger.info("Building " + this.getClass().getName());

    // Setup and start a new game
    setupGame();

    // Setting up the pane
    var pane = setupStackPane("challenge-background");

    // Layout generation
    generateBorder(pane);
    generateSideBar();
    generateTopBar();

    // UI elements generation
    generateScoreBox();
    generateMultiplierBox();
    generateTitle("Online Mode");
    generateLivesBox();
    generateLeaderboard();
    generateLevel();
    generateSideBoards();
    generateTimerBar();

    // Handle block on game board being clicked
    board.setOnRightClick(this::rotateBlock);
    board.setOnBlockClick(this::blockClicked);
  }

  /** Generate the leaderboard UI element. */
  private void generateLeaderboard() {
    Text leaderboardLabel = new Text("Leaderboard");
    leaderboardLabel.getStyleClass().add("heading");
    sideBar.getChildren().add(leaderboardLabel);

    // Set up the remote scores
    remoteScoreList = FXCollections.observableArrayList(remoteScores);
    SimpleListProperty<Pair<String, Integer>> scoreWrapper =
        new SimpleListProperty<>(remoteScoreList);

    // Set up the leaderboard UI element
    leaderboard = new ScoreField();
    leaderboard.getStyleClass().add("leaderboard");
    leaderboard.setReveal(true);
    leaderboard.scoreProperty().bind(scoreWrapper);
    leaderboard.nameProperty().bind(name);
    sideBar.getChildren().add(leaderboard);
  }

  /**
   * Send message to the server.
   *
   * @param message the message to send
   */
  protected void sendMessage(String message) {
    communicator.send(message);
  }

  public abstract void setupGame();
}
