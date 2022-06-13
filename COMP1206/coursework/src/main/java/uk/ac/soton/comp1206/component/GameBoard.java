package uk.ac.soton.comp1206.component;

import javafx.scene.input.MouseButton;
import javafx.scene.layout.GridPane;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import java.util.Collection;

import uk.ac.soton.comp1206.event.BlockClickedListener;
import uk.ac.soton.comp1206.event.RightClickListener;
import uk.ac.soton.comp1206.game.Game;
import uk.ac.soton.comp1206.game.Grid;

/**
 * A GameBoard is a visual component to represent the visual GameBoard. It extends a GridPane to
 * hold a grid of GameBlocks.
 *
 * <p>The GameBoard can hold an internal grid of its own, for example, for displaying an upcoming
 * block. It is also linked to an external grid, for the main game board.
 *
 * <p>The GameBoard is only a visual representation and should not contain game logic or model logic
 * in it, which should take place in the Grid.
 */
public class GameBoard extends GridPane {

  private static final Logger logger = LogManager.getLogger(GameBoard.class);

  /** Number of columns in the board. */
  private final int cols;

  /** Number of rows in the board. */
  private final int rows;

  /** The visual width of the board - has to be specified due to being a Canvas. */
  private final double width;

  /** The visual height of the board - has to be specified due to being a Canvas. */
  private final double height;

  /** The grid this GameBoard represents. */
  final Grid grid;

  /** The blocks inside the grid. */
  GameBlock[][] blocks;

  /** The currently hovered block in the game board. */
  private GameBlock hoveredBlock;

  /** The listener to call when a specific block is clicked. */
  private BlockClickedListener blockClickedListener;

  protected RightClickListener onRightClicked;

  private Game game;

  /**
   * Create a new GameBoard, based off a given grid, with a visual width and height.
   *
   * @param grid linked grid
   * @param width the visual width
   * @param height the visual height
   */
  public GameBoard(Grid grid, double width, double height) {
    this.cols = grid.getCols();
    this.rows = grid.getRows();
    this.width = width;
    this.height = height;
    this.grid = grid;

    // Build the GameBoard
    build();
  }

  /**
   * Create a new GameBoard with its own internal grid, specifying the number of columns and rows,
   * along with the visual width and height.
   *
   * @param cols number of columns for internal grid
   * @param rows number of rows for internal grid
   * @param width the visual width
   * @param height the visual height
   */
  public GameBoard(int cols, int rows, double width, double height) {
    this.cols = cols;
    this.rows = rows;
    this.width = width;
    this.height = height;
    this.grid = new Grid(cols, rows);

    // Build the GameBoard
    build();
  }

  /**
   * Get a specific block from the GameBoard, specified by its row and column.
   *
   * @param x column
   * @param y row
   * @return game block at the given column and row
   */
  public GameBlock getBlock(int x, int y) {
    return blocks[x][y];
  }

  /**
   * Set the game object related to the game board.
   *
   * @param game the game to manage
   */
  public void setGame(Game game) {
    this.game = game;
  }

  /** Build the GameBoard by creating a block at every x and y column and row. */
  protected void build() {
    logger.info("Building grid: {} x {}", cols, rows);

    this.getStyleClass().add("gameboard");

    setMaxWidth(width);
    setMaxHeight(height);

    setGridLinesVisible(true);

    blocks = new GameBlock[cols][rows];

    for (var y = 0; y < rows; y++) {
      for (var x = 0; x < cols; x++) {
        createBlock(x, y);
      }
    }
  }

  /**
   * Create a block at the given x and y position in the GameBoard
   *
   * @param x column
   * @param y row
   */
  protected void createBlock(int x, int y) {
    var blockWidth = width / cols;

    // Create a new GameBlock UI component
    var block = new GameBlock(x, y, blockWidth);

    // Add to the GridPane
    add(block, x, y);

    // Add to our block directory
    blocks[x][y] = block;

    // Link the GameBlock component to the corresponding value in the Grid
    block.bind(grid.getGridProperty(x, y));

    // Actions for the block being left or right-clicked
    block.setOnMouseClicked(
        (e) -> {
          if (e.getButton() == MouseButton.PRIMARY) {
            this.blockClicked(block);
          } else {
            this.rightClick();
          }
        });

    // Trigger hover methods when the mouse enters or exits a game block
    block.setOnMouseEntered((e) -> hovered(block));
    block.setOnMouseExited((e) -> resetHovered());
  }

  /**
   * Set the listener to handle an event when a block is clicked.
   *
   * @param listener listener to add
   */
  public void setOnBlockClick(BlockClickedListener listener) {
    this.blockClickedListener = listener;
  }

  /** Reset all game blocks to not hovered. */
  public void resetHovered() {
    // logger.info("Exited block {}", this);
    try {
      for (GameBlock[] gameBlocks : blocks) {
        for (GameBlock gameBlock : gameBlocks) {
          gameBlock.setHovered(false, false);
        }
      }
    } catch (Exception ignored) {
    }
  }

  /**
   * Reset all game board blocks hovered status.
   *
   * <p>Redraw the currently hovered block and its piece shadow.
   *
   * @param block the block to refresh for
   */
  public void refreshHovered(GameBlock block) {
    resetHovered();
    hovered(block);
  }

  /**
   * Right click handler setter.
   *
   * @param handler the handler to set
   */
  public void setOnRightClick(RightClickListener handler) {
    onRightClicked = handler;
  }

  /**
   * Set the blocks to fade from the game.
   *
   * @param blocks The list of blocks to fade out
   */
  public void fade(Collection<GameBlockCoordinate> blocks) {
    for (GameBlockCoordinate block : blocks) {
      getBlock(block.getX(), block.getY()).fadeOut();
    }
  }

  /**
   * Get the currently hovered game block in the game board.
   *
   * @return the game block hovered
   */
  public GameBlock getHoveredBlock() {
    return hoveredBlock;
  }

  /**
   * Triggered when a block is clicked. Call the attached listener.
   *
   * @param block block clicked on
   */
  private void blockClicked(GameBlock block) {
    // logger.info("Block clicked: {}", block);
    if (blockClickedListener != null) {
      blockClickedListener.blockClicked(block);
    }
  }

  /** Triggered when a block is clicked. Call the attached listener. */
  private void rightClick() {
    if (this.onRightClicked != null) {
      this.onRightClicked.rightClicked();
    }
  }

  /**
   * Set the shadow for the blocks around the currently hovered game block.
   *
   * @param block the block currently hovered
   */
  private void hovered(GameBlock block) {
    hoveredBlock = block;

    if (game != null && game.getCurrentPiece() != null) {
      var piece = game.getCurrentPiece();
      var pieceBlocks = piece.getBlocks();

      for (int x = 0; x < pieceBlocks.length; ++x) {
        for (int y = 0; y < pieceBlocks[x].length; ++y) {
          // Block's X and Y positions
          int blockX = block.getX();
          int blockY = block.getY();

          // Block position converted to game board positions
          int potentialX = blockX + x - 1;
          int potentialY = blockY + y - 1;

          // Check that the block has a piece part on it and is in the grid's bounds
          if (pieceBlocks[x][y] != 0 && checkBounds(potentialX, potentialY)) {
            // logger.info("{} {}", block.getX(), block.getY());
            blocks[potentialX][potentialY].setHovered(
                true, grid.canPlayPiece(piece, blockX - 1, blockY - 1));
          }
        }
      }
    }
  }

  /**
   * Check the piece is in the game board bounds
   *
   * @param potentialX the X position the piece would drop
   * @param potentialY the Y position the piece would drop
   * @return if the piece is inside or outside the bounds
   */
  private boolean checkBounds(int potentialX, int potentialY) {
    if (potentialX >= 0 && potentialX <= cols - 1) {
      return potentialY >= 0 && potentialY <= rows - 1;
    }
    return false;
  }

  /**
   * Number of columns getter.
   *
   * @return number of columns
   */
  public int getCols() {
    return cols;
  }

  /**
   * Number of rows getter.
   *
   * @return number of rows
   */
  public int getRows() {
    return rows;
  }
}
