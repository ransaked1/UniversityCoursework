package uk.ac.soton.comp1206.scene.base;

import javafx.animation.KeyFrame;
import javafx.animation.KeyValue;
import javafx.animation.Timeline;
import javafx.beans.property.*;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.scene.layout.StackPane;
import javafx.scene.paint.Color;
import javafx.scene.shape.Rectangle;
import javafx.util.Duration;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.soton.comp1206.component.GameBlock;
import uk.ac.soton.comp1206.component.GameBoard;
import uk.ac.soton.comp1206.ui.GameWindow;

/** A base scene used for Game Scenes. Handles common backend and UI updates. */
public abstract class BaseGameScene extends BaseScene {

  private static final Logger logger = LogManager.getLogger(BaseGameScene.class);

  /** UI boards */
  protected GameBoard board;

  /** Bound properties */
  protected final IntegerProperty score = new SimpleIntegerProperty(0);
  protected final IntegerProperty highscore = new SimpleIntegerProperty(0);
  protected final StringProperty mult = new SimpleStringProperty("X1");

  /** Timer UI */
  protected StackPane timerStack;

  protected Rectangle timer;

  /**
   * Create a new scene, passing in the GameWindow the scene will be displayed in
   *
   * @param gameWindow the game window
   */
  public BaseGameScene(GameWindow gameWindow) {
    super(gameWindow);
  }

  /**
   * Handle key press events
   *
   * @param keyEvent the key event object
   */
  protected void handleKeyPress(KeyEvent keyEvent) {
    var keyCode = keyEvent.getCode();

    // Keyboard mode positions
    int keyboardX = board.getHoveredBlock().getX();
    int keyboardY = board.getHoveredBlock().getY();

    // Exit game scene with keyboard
    checkGameExit(keyCode);

    // Place a game piece
    if (keyCode.equals(KeyCode.ENTER) || keyCode.equals(KeyCode.X)) {
      blockClicked(board.getBlock(keyboardX, keyboardY));
    }

    // Move piece to the left
    if ((keyCode.equals(KeyCode.A) || keyCode.equals(KeyCode.LEFT)) && keyboardX > 0) {
      keyboardX--;
    }

    // Move piece to the right
    if ((keyCode.equals(KeyCode.D) || keyCode.equals(KeyCode.RIGHT))
        && keyboardX < game.getCols() - 1) {
      keyboardX++;
    }

    // Move piece down
    if ((keyCode.equals(KeyCode.S) || keyCode.equals(KeyCode.DOWN))
        && keyboardY < game.getRows() - 1) {
      keyboardY++;
    }

    // Move piece up
    if ((keyCode.equals(KeyCode.W) || keyCode.equals(KeyCode.UP)) && keyboardY > 0) {
      keyboardY--;
    }

    // Swap pieces
    if (keyCode.equals(KeyCode.SPACE) || keyCode.equals(KeyCode.R)) {
      swapBlock();
    }

    // Rotate left (triple rotation to right to achieve it)
    if (keyCode.equals(KeyCode.Q)
        || keyCode.equals(KeyCode.Z)
        || keyCode.equals(KeyCode.OPEN_BRACKET)) {
      rotateBlock(3);
    }

    // Rotate right
    if (keyCode.equals(KeyCode.E)
        || keyCode.equals(KeyCode.C)
        || keyCode.equals(KeyCode.CLOSE_BRACKET)) {
      rotateBlock();
    }

    // Discard block and reset game loop
    if (keyCode.equals(KeyCode.V)) {
      game.gameLoop();
    }

    // Refresh hovered block
    board.refreshHovered(board.getBlock(keyboardX, keyboardY));
  }

  /**
   * Change value of the score in the game UI
   *
   * @param oldVal the old score value
   * @param newVal the new score value
   */
  protected void setScore(Number oldVal, Number newVal) {
    logger.info("Score is now {}", newVal);

    if (newVal.intValue() > highscore.get()) {
      highscore.set(newVal.intValue());
    }

    // Set up the animation for score increase
    var startValueKeyFrame = new KeyFrame(Duration.ZERO, new KeyValue(score, oldVal));
    var endValueKeyFrame = new KeyFrame(new Duration(200), new KeyValue(score, newVal));
    var timeline = new Timeline(startValueKeyFrame, endValueKeyFrame);

    timeline.play();
  }

  /**
   * Manage the game loop bar animation.
   *
   * @param nextLoop the length of the game loop
   */
  protected void gameLoop(int nextLoop) {
    var greenKeyFrame =
        new KeyFrame(Duration.ZERO, new KeyValue(timer.fillProperty(), Color.GREEN));

    var startingLengthKeyFrame =
        new KeyFrame(Duration.ZERO, new KeyValue(timer.widthProperty(), timerStack.getWidth()));

    var yellowKeyFrame =
        new KeyFrame(
            new Duration((double) nextLoop * 0.5),
            new KeyValue(timer.fillProperty(), Color.YELLOW));

    var redKeyFrame =
        new KeyFrame(
            new Duration((double) nextLoop * 0.75), new KeyValue(timer.fillProperty(), Color.RED));

    var endingLengthKeyFrame =
        new KeyFrame(new Duration(nextLoop), new KeyValue(timer.widthProperty(), 0));

    // Setting up the timeline for the timer bar
    Timeline timeline =
        new Timeline(
            greenKeyFrame,
            startingLengthKeyFrame,
            yellowKeyFrame,
            redKeyFrame,
            endingLengthKeyFrame);

    // Start the animation
    timeline.play();
  }

  protected abstract void checkGameExit(KeyCode keyCode);

  protected abstract void rotateBlock(int rotations);

  protected abstract void swapBlock();

  protected abstract void rotateBlock();

  protected abstract void blockClicked(GameBlock gameBlock);
}
