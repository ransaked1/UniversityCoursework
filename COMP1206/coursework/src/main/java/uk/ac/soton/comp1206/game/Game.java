package uk.ac.soton.comp1206.game;

import static javafx.application.Platform.runLater;

import javafx.beans.property.IntegerProperty;
import javafx.beans.property.SimpleIntegerProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.util.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

import uk.ac.soton.comp1206.component.GameBlock;
import uk.ac.soton.comp1206.component.GameBlockCoordinate;
import uk.ac.soton.comp1206.event.LineClearedListener;
import uk.ac.soton.comp1206.event.GameLoopListener;
import uk.ac.soton.comp1206.event.GameOverListener;
import uk.ac.soton.comp1206.event.NextPieceListener;
import uk.ac.soton.comp1206.util.Constants;
import uk.ac.soton.comp1206.util.Multimedia;

/**
 * The Game class handles the main logic, state and properties of the TetrECS game. Methods to
 * manipulate the game state and to handle actions made by the player should take place inside this
 * class.
 */
public abstract class Game {

  private static final Logger logger = LogManager.getLogger(Game.class);

  protected final ArrayList<Pair<String, Integer>> scores = new ArrayList<>();

  protected final int rows;
  protected final int cols;
  protected final Grid grid;

  /** Game pieces */
  private GamePiece currentPiece;

  protected GamePiece followingPiece;

  /** Listeners */
  private NextPieceListener followingPieceListener = null;

  private LineClearedListener lineClearedListener = null;
  private GameLoopListener gameLoopListener = null;
  private GameOverListener gameOverListener = null;

  /** Bindable game properties */
  protected final IntegerProperty score = new SimpleIntegerProperty(0);

  protected final IntegerProperty level = new SimpleIntegerProperty(1);
  protected final IntegerProperty lives = new SimpleIntegerProperty(3);
  protected final IntegerProperty mult = new SimpleIntegerProperty(1);
  private final StringProperty multString = new SimpleStringProperty("");
  private final StringProperty name = new SimpleStringProperty();

  /** Game loop */
  private final ScheduledExecutorService executor;

  private ScheduledFuture<?> nextLoop;
  private ScheduledFuture<?> lowTimeSound1;
  private ScheduledFuture<?> lowTimeSound2;
  private ScheduledFuture<?> lowTimeSound3;

  /**
   * Create a new game with the specified rows and columns. Creates a corresponding grid model.
   *
   * @param cols number of columns
   * @param rows number of rows
   */
  public Game(int cols, int rows) {
    this.cols = cols;
    this.rows = rows;

    // Create a new grid model to represent the game state
    grid = new Grid(cols, rows);

    // Create single threaded executor for the game
    executor = Executors.newSingleThreadScheduledExecutor();
  }

  /** Dummy constructor for when the leaderboard scene is opened. */
  public Game() {
    rows = 0;
    cols = 0;
    executor = null;
    grid = new Grid(cols, rows);
  }

  /** Start the game */
  public void start() {
    logger.info("Starting game");
    initialiseGame();
    startGameLoop();
  }

  /** Initialise a new game and set up anything that needs to be done at the start */
  public void initialiseGame() {
    logger.info("Initialising game");

    // Initializing bindable values
    score.set(0);
    level.set(0);
    lives.set(3);
    mult.set(1);

    // Initializing pieces
    followingPiece = spawnPiece();
    nextPiece();

  }

  /**
   * Move the following piece to the current one.
   *
   * <p>Generate a new game piece and set it to the following piece.
   */
  public void nextPiece() {
    currentPiece = followingPiece;
    followingPiece = spawnPiece();

    logger.info("Current piece: " + currentPiece);
    logger.info("Next piece: " + followingPiece);

    if (followingPieceListener != null) {
      followingPieceListener.nextPiece(currentPiece);
    }
  }

  /**
   * Swap game pieces.
   */
  public void swapCurrentPiece() {
    GamePiece tmp = currentPiece;
    currentPiece = followingPiece;
    followingPiece = tmp;
  }

  /**
   * Rotate the current piece.
   *
   * @param rotations amount of times to rotate clock wise.
   */
  public void rotateCurrentPiece(int rotations) {
    currentPiece.rotate(rotations);
  }

  /**
   * Handle what should happen when a particular block is clicked
   *
   * @param gameBlock the block that was clicked
   */
  public boolean blockClicked(GameBlock gameBlock) {
    // Get the position of this block
    int x = gameBlock.getX();
    int y = gameBlock.getY();

    // logger.info("Block clicked: {},{}", x, y);

    // Play a piece and handle the game changes
    if (currentPiece != null) {
      if (grid.playPiece(currentPiece, x, y)) {
        afterPiece();
        nextPiece();
        return true;
      }
    }
    //logger.error("Cant add piece at {}, {}", x, y);
    return false;
  }

  /** Check if there are full rows or columns in the game space and manage their clearing */
  public void afterPiece() {
    int lines = 0;

    // Sets keeping the
    HashSet<IntegerProperty> clear = new HashSet<>();
    HashSet<GameBlockCoordinate> clearBlocks = new HashSet<>();

    // Count the lines to clear and add them to the clear sets
    lines += countClearRows(lines, clear, clearBlocks);
    lines = countClearColumns(lines, clear, clearBlocks);

    // Calculate the score, multipliers, level and clean grid
    if (lines != 0) {
      score(lines, clear, clearBlocks);
    } else {
      resetMultiplier();
    }
  }

  /**
   * Calculate the score for a specific number of lines and trigger UI update
   *
   * @param lines the number of lines to be cleared
   * @param clear the grid cells to be cleared
   * @param clearBlocks the UI cells to be cleared
   */
  private void score(
      int lines, HashSet<IntegerProperty> clear, HashSet<GameBlockCoordinate> clearBlocks) {
    // Updating values for score, multiplier and level
    increaseScore(lines * clear.size() * 10 * mult.get());
    mult.set(mult.get() + 1);
    multString.set("X" + mult.get());
    level.set(score.get() / 1000);

    cleanGrid(clear);

    if (lineClearedListener != null) {
      lineClearedListener.lineCleared(clearBlocks);
    }
  }

  /**
   * Set all grid cells to be cleared to 0
   *
   * @param clear the cells to be cleared
   */
  private void cleanGrid(HashSet<IntegerProperty> clear) {
    for (IntegerProperty square : clear) {
      square.set(0);
    }
  }

  /** Reset the game multiplier */
  private void resetMultiplier() {
    if (mult.get() > 1) {
      logger.info("Multiplier reset");
      mult.set(1);
      multString.set("X1");
    }
  }

  /**
   * Detect the columns or rows that are full and add them to the clearance sets
   *
   * @param lines the number of full rows/columns
   * @param clear set of cells to clean in the grid
   * @param clearBlocks set of cells to clean in the UI
   * @return the lines counted
   */
  private int countClearRows(
      int lines, HashSet<IntegerProperty> clear, HashSet<GameBlockCoordinate> clearBlocks) {
    int total;

    for (int y = 0; y < rows; ++y) {
      total = rows; // Set total to number of cells in row

      // Decrease total if cell is not empty
      for (int x = 0; x < cols; ++x) {
        if (grid.get(x, y) != 0) {
          total--;
        }
      }

      // If total is 0 that means the row is full
      if (total == 0) {
        lines++;
        for (int x = 0; x < cols; ++x) {
          addToClearSets(clear, clearBlocks, x, y);
        }
      }
    }
    return lines;
  }

  /**
   * Detect the columns that are full and add them to the clearance sets
   *
   * @param lines the number of full columns
   * @param clear set of cells to clean in the grid
   * @param clearBlocks set of cells to clean in the UI
   * @return the lines counted
   */
  private int countClearColumns(
      int lines, HashSet<IntegerProperty> clear, HashSet<GameBlockCoordinate> clearBlocks) {
    int total;

    for (int x = 0; x < cols; ++x) {
      total = rows; // Set total to number of cells in column

      // Decrease total if cell is not empty
      for (int y = 0; y < rows; ++y) {
        if (grid.get(x, y) != 0) {
          total--;
        }
      }

      // If total is 0 that means the column is full
      if (total == 0) {
        lines++;
        for (int y = 0; y < rows; ++y) {
          addToClearSets(clear, clearBlocks, x, y);
        }
      }
    }
    return lines;
  }

  /**
   * Adds block at position x,y to the clearance sets
   *
   * @param clear set of cells to clean in the grid
   * @param clearBlocks set of cells to clean in the UI
   * @param first coordinate of cell
   * @param second coordinate of cell
   */
  private void addToClearSets(
      HashSet<IntegerProperty> clear,
      HashSet<GameBlockCoordinate> clearBlocks,
      int first,
      int second) {
    clear.add(grid.getGridProperty(first, second));
    clearBlocks.add(new GameBlockCoordinate(first, second));
  }

  /** Stopping the game */
  public void stop() {
    logger.info("Stopping game!");
    executor.shutdownNow();
  }

  /**
   * Starting the game loop.
   */
  public void startGameLoop() {
    nextLoop = executor.schedule(this::gameLoop, getTimerDelay(), TimeUnit.MILLISECONDS);
    lowTimerSet();
    if (gameLoopListener != null) {
      gameLoopListener.gameLoop(getTimerDelay());
    }
  }

  /**
   * Play a sound when the time is getting low.
   */
  private void playLowTime() {
    Multimedia.playAudio("low_time.wav");
  }

  /**
   * Restarting the game loop.
   */
  public void restartGameLoop() {
    nextLoop.cancel(false);
    lowTimerCancel();
    startGameLoop();
  }

  /**
   * Cancel the timers so sounds are not played.
   */
  private void lowTimerCancel() {
    lowTimeSound1.cancel(false);
    lowTimeSound2.cancel(false);
    lowTimeSound3.cancel(false);
  }

  /**
   * Stopping the game when it is over.
   */
  private void gameOver() {
    logger.info("Game over!");
    if (gameOverListener != null) {
      runLater(() -> gameOverListener.gameOver());
    }
  }

  /**
   * Finishing a game loop cycle.
   */
  public void gameLoop() {
    // Reset the multiplier
    if (mult.get() > 1) {
      logger.info("Multiplier reset to 1");
      mult.set(1);
      multString.set("X1");
    }

    // Decrease the live count
    decreaseLives();

    // Generate the next piece
    nextPiece();

    // Generate the timer for the next game loop
    int nextTimer = getTimerDelay();

    if (gameLoopListener != null) {
      gameLoopListener.gameLoop(nextTimer);
    }

    // Cancel the game loop timers
    nextLoop.cancel(false);
    lowTimerCancel();

    // Set the game loop timers
    nextLoop = executor.schedule(this::gameLoop, nextTimer, TimeUnit.MILLISECONDS);
    lowTimerSet();
  }

  /**
   * Setting timers for the low time.
   */
  private void lowTimerSet() {
    lowTimeSound1 =
        executor.schedule(
            this::playLowTime, (long) (getTimerDelay() * 0.75), TimeUnit.MILLISECONDS);
    lowTimeSound2 =
        executor.schedule(
            this::playLowTime, (long) (getTimerDelay() * 0.85), TimeUnit.MILLISECONDS);
    lowTimeSound3 =
        executor.schedule(
            this::playLowTime, (long) (getTimerDelay() * 0.95), TimeUnit.MILLISECONDS);
  }

  /**
   * Decrease the amount of lives and trigger the game over method when it is bellow 0.
   */
  private void decreaseLives() {
    if (lives.get() > 0) {
      lives.set(lives.get() - 1);
    } else {
      gameOver();
    }
  }

  /**
   * Get the timer delay for the next game loop cycle.
   *
   * Decides between the calculated time and the minimum time.
   *
   * @return the time in milliseconds
   */
  public int getTimerDelay() {
    int time = Constants.MAXIMUM_TIME - Constants.TIME_DECREASE_STEP * level.get();
    return Math.max(time, Constants.MINIMUM_TIME);
  }

  /**
   * Creates a new game piece and returns it. Needs different implementations in single and
   * multiplayer
   *
   * @return a new random game piece
   */
  public abstract GamePiece spawnPiece();

  /**
   * Get the grid model inside this game representing the game state of the board
   *
   * @return game grid model
   */
  public Grid getGrid() {
    return grid;
  }

  /**
   * Get the number of columns in this game
   *
   * @return number of columns
   */
  public int getCols() {
    return cols;
  }

  /**
   * Get the number of rows in this game
   *
   * @return number of rows
   */
  public int getRows() {
    return rows;
  }

  /**
   * Increases the score property by a given amount
   *
   * @param amount to increase by
   */
  public void increaseScore(int amount) {
    score.set(score.add(amount).get());
  }

  public ArrayList<Pair<String, Integer>> getScores() {
    return this.scores;
  }

  public void setOnGameLoop(GameLoopListener listener) {
    gameLoopListener = listener;
  }

  public void setOnGameOver(GameOverListener listener) {
    gameOverListener = listener;
  }

  public void setOnLineCleared(LineClearedListener listener) {
    lineClearedListener = listener;
  }

  public GamePiece getCurrentPiece() {
    return currentPiece;
  }

  public GamePiece getFollowingPiece() {
    return followingPiece;
  }

  public int getScore() {
    return score.get();
  }

  public IntegerProperty scoreProperty() {
    return score;
  }

  public IntegerProperty levelProperty() {
    return level;
  }

  public IntegerProperty livesProperty() {
    return lives;
  }

  public StringProperty multProperty() {
    return multString;
  }

  public StringProperty nameProperty() {
    return name;
  }

  public void setNextPieceListener(NextPieceListener listener) {
    followingPieceListener = listener;
  }
}
