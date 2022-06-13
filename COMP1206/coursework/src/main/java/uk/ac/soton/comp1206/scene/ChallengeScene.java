package uk.ac.soton.comp1206.scene;

import javafx.beans.value.ObservableValue;
import javafx.scene.input.KeyCode;
import javafx.util.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.soton.comp1206.component.GameBlock;
import uk.ac.soton.comp1206.component.GameBlockCoordinate;
import uk.ac.soton.comp1206.game.GamePiece;
import uk.ac.soton.comp1206.game.SingleplayerGame;
import uk.ac.soton.comp1206.scene.build.ChallengeSceneBuilder;
import uk.ac.soton.comp1206.ui.GameWindow;
import uk.ac.soton.comp1206.util.Multimedia;
import uk.ac.soton.comp1206.util.Storage;

import java.util.ArrayList;
import java.util.Collection;

/**
 * The Single Player challenge scene. Holds the UI for the single player challenge mode in the game.
 */
public class ChallengeScene extends ChallengeSceneBuilder {

  private static final Logger logger = LogManager.getLogger(ChallengeScene.class);

  /**
   * Create a new Single Player challenge scene
   *
   * @param gameWindow the Game Window
   */
  public ChallengeScene(GameWindow gameWindow) {
    super(gameWindow);
    logger.info("Creating Challenge Scene");
  }

  /** Initialise the scene and start the game */
  @Override
  public void initialise() {
    logger.info("Initialising Challenge");
    Multimedia.startBackgroundMusic("game.mp3", true);

    // Initializing listeners
    game.scoreProperty().addListener((observable, oldVal, newVal) -> setScore(oldVal, newVal));
    game.livesProperty().addListener(ChallengeScene::setLives);
    game.levelProperty().addListener(ChallengeScene::playLevelUp);
    game.multProperty().addListener(this::setMultiplier);
    game.setNextPieceListener(this::nextPiece);
    scene.setOnKeyPressed(this::handleKeyPress);
    game.setOnGameLoop(this::gameLoop);
    game.setOnGameOver(this::gameOver);
    game.setOnLineCleared(this::lineCleared);

    // Initialize the score list
    ArrayList<Pair<String, Integer>> scores = Storage.loadScores();
    highscore.set((scores.get(0)).getValue());

    game.start();
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
    }
  }

  /**
   * Swapping the current and next piece.
   */
  @Override
  protected void swapBlock() {
    logger.info("Swapping block");
    Multimedia.playAudio("swap.wav");
    game.swapCurrentPiece();
    currentPiece.setPiece(game.getCurrentPiece());
    nextPiece.setPiece(game.getFollowingPiece());
  }

  @Override
  protected void swapBlock(GameBlock gameBlock) {
    swapBlock();
  }

  /**
   * Default to rotateBlock(int rotations) when rotating over a game block
   *
   * @param gameBlock the game block rotation is over
   */
  @Override
  protected void rotateBlock(GameBlock gameBlock) {
    rotateBlock(1);
  }

  /** Default to rotateBlock(int rotations) when rotating over any other ui elements */
  @Override
  public void rotateBlock() {
    rotateBlock(1);
  }

  /**
   * Rotate the current piece once in the right direction
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

  /** Stop the game and music */
  private void endGame() {
    logger.info("Ending game");
    game.stop();
    Multimedia.stopAll();
  }

  /**
   * Handle when a block is clicked
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

  /** Set up the game object and model */
  @Override
  public void setupGame() {
    logger.info("Starting a new challenge");
    // Start new game
    game = new SingleplayerGame(5, 5);
  }

  /**
   * Trigger the blocks fade when lines are cleared.
   *
   * @param blocks the blocks to be cleared
   */
  private void lineCleared(Collection<GameBlockCoordinate> blocks) {
    // Make the blocks to be cleared to fade
    board.fade(blocks);
    Multimedia.playAudio("clearblocks.wav");
  }

  /**
   * Change value of the lives in the game UI.
   *
   * @param observable the observable object
   * @param oldVal the old lives value
   * @param newVal the new lives value
   */
  private static void setLives(
      ObservableValue<? extends Number> observable, Number oldVal, Number newVal) {
    // Play sound when live count changes
    Multimedia.playAudio(oldVal.intValue() > newVal.intValue() ? "lifelose.wav" : "lifegain.wav");
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
   * Play a sound when leveling up.
   *
   * @param observable the observable object
   * @param oldVal the old lives value
   * @param newVal the new lives value
   */
  private static void playLevelUp(
      ObservableValue<? extends Number> observable, Number oldVal, Number newVal) {
    // Check that the level value changed
    if (newVal.intValue() != oldVal.intValue()) {
      Multimedia.playAudio("levelup.wav");
    }
  }

  /**
   * Replace currentPiece with nextPiece and set a new piece for nextPiece.
   *
   * @param next the piece to add to the UI
   */
  private void nextPiece(GamePiece next) {
    // logger.info("Next piece to place: " + nextPiece);
    currentPiece.setPiece(next);
    nextPiece.setPiece(game.getFollowingPiece());
    board.resetHovered();
  }

  /** End the game on game over and show the score scene. */
  private void gameOver() {
    endGame();
    gameWindow.startScores(game);
  }
}
