package uk.ac.soton.comp1206.scene;

import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.beans.value.ObservableValue;
import javafx.scene.input.KeyCode;
import javafx.util.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.soton.comp1206.component.GameBlock;
import uk.ac.soton.comp1206.component.GameBlockCoordinate;
import uk.ac.soton.comp1206.game.GamePiece;
import uk.ac.soton.comp1206.game.MultiplayerGame;
import uk.ac.soton.comp1206.scene.build.MultiplayerGameSceneBuilder;
import uk.ac.soton.comp1206.ui.GameWindow;
import uk.ac.soton.comp1206.util.Multimedia;

import java.util.ArrayList;
import java.util.Collection;

import static javafx.application.Platform.*;

/** The Multiplayer scene. Holds the UI for the multiplayer game mode in the game. */
public class MultiplayerGameScene extends MultiplayerGameSceneBuilder {

  private static final Logger logger = LogManager.getLogger(MultiplayerGameScene.class);

  private final ArrayList<Pair<String, Integer>> remoteScores = new ArrayList<>();
  private final StringProperty name = new SimpleStringProperty();

  /**
   * Create a new Multi Player challenge scene
   *
   * @param gameWindow the Game Window
   */
  public MultiplayerGameScene(GameWindow gameWindow) {
    super(gameWindow);
  }

  /** Initialize a multiplayer game. */
  public void initialise() {
    logger.info("Initialised {}", this.getClass());
    Multimedia.startBackgroundMusic("game_online.mp3", true);

    // Setting up listeners
    game.scoreProperty().addListener((observable1, oldVal, newVal) -> setScore(oldVal, newVal));
    game.setOnLineCleared(this::lineCleared);
    game.setOnGameLoop(this::gameLoop);
    game.multProperty().addListener(this::setMultiplier);
    game.setNextPieceListener(this::nextPiece);
    scene.setOnKeyPressed(this::handleKeyPress);

    // Start the game
    game.start();

    // Listen to the server and update the name and scores
    communicator.addListener((message) -> runLater(() -> receiveMessage(message.trim())));
    updateName();
    updateScores();

    // Binding UI elements
    bindProperties();
  }

  /** Binding game variables to the UI. */
  private void bindProperties() {
    game.scoreProperty()
        .addListener((observable, oldValue, newValue) -> sendMessage("SCORE " + newValue));

    game.livesProperty()
        .addListener(
            (observable, oldValue, newValue) -> {
              sendMessage("LIVES " + newValue);
              if (oldValue.intValue() > newValue.intValue()) {
                Multimedia.playAudio("lifelose.wav");
              } else {
                Multimedia.playAudio("lifegain.wav");
              }
            });

    // Play sound on level up
    game.levelProperty()
        .addListener(
            (observable, oldValue, newValue) -> {
              if (newValue.intValue() > oldValue.intValue()) {
                Multimedia.playAudio("levelup.wav");
              }
            });

    // On game over, end the game, start the score scene and leave the channel
    game.setOnGameOver(
        () -> {
          endGame();
          gameWindow.startScores(game);
          communicator.send("PART");
        });
  }

  /** Request name update. */
  private void updateName() {
    sendMessage("NICK");
  }

  /** Request scores from the server. */
  private void updateScores() {
    sendMessage("SCORES");
  }

  /**
   * Receive and parse message from the server.
   *
   * @param message the message to be parsed
   */
  private void receiveMessage(String message) {
    logger.info("Received message: {}", message);
    String[] components = message.split(" ", 2);
    String command = components[0];
    String data;

    if (command.equals("SCORES") && components.length > 1) {
      data = components[1];
      receiveScores(data);
    } else if (command.equals("NICK") && components.length > 1) {
      data = components[1];
      if (!data.contains(":")) {
        this.setName(components[1]);
      }
    }
  }

  /**
   * Set the player name.
   *
   * @param name the name to set
   */
  private void setName(String name) {
    logger.info("Name set: " + name);
    this.name.set(name);
    game.nameProperty().set(name);
  }

  /**
   * Parse the scores from the server.
   *
   * @param data the raw response from the server
   */
  private void receiveScores(String data) {
    // logger.info("Received scores: {}", data);
    remoteScores.clear();
    String[] scoreLines = data.split("\\R");

    for (String scoreLine : scoreLines) {
      String[] components = scoreLine.split(":");
      String player = components[0];
      int score = Integer.parseInt(components[1]);

      logger.info("Received score: {} = {}", player, score);
      String lives = components[2];
      if (lives.equals("DEAD")) {
        leaderboard.kill(player);
      }

      remoteScores.add(new Pair<>(player, score));
    }

    remoteScores.sort((a, b) -> b.getValue().compareTo(a.getValue()));
    remoteScoreList.clear();
    remoteScoreList.addAll(this.remoteScores);
  }

  /** Stop the game and music */
  private void endGame() {
    logger.info("Ending game");
    game.stop();
    Multimedia.stopAll();
  }

  /** Set up the game object and model */
  public void setupGame() {
    logger.info("Starting a new multiplayer game");
    game = new MultiplayerGame(communicator, 5, 5);
  }

  /**
   * Trigger game end when ESC button is pressed.
   *
   * @param keyCode the key pressed
   */
  protected void checkGameExit(KeyCode keyCode) {
    if (keyCode.equals(KeyCode.ESCAPE)) {
      endGame();
      gameWindow.startMenu();
      communicator.send("PART");
    }
  }

  /**
   * Trigger the blocks fade when lines are cleared.
   *
   * @param blocks the blocks to be cleared
   */
  private void lineCleared(Collection<GameBlockCoordinate> blocks) {
    board.fade(blocks);
    Multimedia.playAudio("clearblocks.wav");
  }

  /**
   * Replace currentPiece with nextPiece and set a new piece for nextPiece
   *
   * @param next the piece to add to the UI
   */
  private void nextPiece(GamePiece next) {
    logger.info("Next piece: " + nextPiece);
    currentPiece.setPiece(next);
    nextPiece.setPiece(game.getFollowingPiece());
    board.resetHovered();
  }

  /** Swapping blocks. */
  @Override
  protected void swapBlock() {
    logger.info("Swapping block");
    Multimedia.playAudio("swap.wav");
    game.swapCurrentPiece();
    currentPiece.setPiece(game.getCurrentPiece());
    nextPiece.setPiece(game.getFollowingPiece());
  }

  /**
   * Change value of the multiplier in the game UI.
   *
   * @param observable the observable object
   * @param oldVal the old multiplier value
   * @param newVal the new multiplier value
   */
  private void setMultiplier(
      ObservableValue<? extends String> observable, String oldVal, String newVal) {
    // Check that the multiplier value changed
    if (!newVal.equals(mult.get())) {
      mult.set(newVal);
      multiplierField.getStyleClass().remove(oldVal);
      multiplierField.getStyleClass().add(newVal);
    }
  }

  /**
   * Swapping blocks with no changes even when hovering over a game block.
   *
   * @param gameBlock the game block hovered
   */
  @Override
  protected void swapBlock(GameBlock gameBlock) {
    swapBlock();
  }

  /**
   * Default to rotateBlock(int rotations) when rotating over a game block.
   *
   * @param gameBlock the game block rotation is over
   */
  @Override
  protected void rotateBlock(GameBlock gameBlock) {
    rotateBlock(1);
  }

  /** Default to rotateBlock(int rotations) when rotating over any other ui elements. */
  @Override
  protected void rotateBlock() {
    rotateBlock(1);
  }

  /**
   * Rotate the current piece once in the right direction.
   *
   * @param repeat the number of times to rotate right (used to left rotating quickly)
   */
  @Override
  protected void rotateBlock(int repeat) {
    logger.info("Rotating block");
    Multimedia.playAudio("rotate.wav");
    game.rotateCurrentPiece(repeat);
    currentPiece.setPiece(game.getCurrentPiece());
    board.refreshHovered(board.getHoveredBlock());
  }

  /**
   * Handle when a block is clicked.
   *
   * @param gameBlock the Game Block that was clocked
   */
  @Override
  protected void blockClicked(GameBlock gameBlock) {
    if (game.blockClicked(gameBlock)) {
      logger.info("Placed {}", gameBlock);
      Multimedia.playAudio("place_success.wav");
      game.restartGameLoop();
    } else {
      logger.info("Unable to place {}", gameBlock);
      Multimedia.playAudio("place_fail.wav");
    }
  }
}
