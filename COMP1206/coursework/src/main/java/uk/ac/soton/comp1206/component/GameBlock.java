package uk.ac.soton.comp1206.component;

import javafx.animation.AnimationTimer;
import javafx.beans.property.IntegerProperty;
import javafx.beans.property.SimpleIntegerProperty;
import javafx.beans.value.ObservableValue;
import javafx.scene.canvas.Canvas;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.paint.*;

import uk.ac.soton.comp1206.util.Constants;

/**
 * The Visual User Interface component representing a single block in the grid.
 *
 * <p>Extends Canvas and is responsible for drawing itself.
 *
 * <p>Displays an empty square (when the value is 0) or a coloured square depending on value.
 *
 * <p>The GameBlock value should be bound to a corresponding block in the Grid model.
 */
public class GameBlock extends Canvas {

  /** Set if the block has a dot. */
  private boolean dot = false;

  /** Fadeout animation timer when the row is cleared. */
  private static AnimationTimer timer = null;

  private final double width;
  private final double height;

  /** Variables storing hover status and color. */
  private boolean hovered;

  private Color hoverColor;

  /** The column this block exists as in the grid . */
  private final int x;

  /** The row this block exists as in the grid. */
  private final int y;

  /** The value of this block (0 = empty, otherwise specifies the colour to render as). */
  private final IntegerProperty value = new SimpleIntegerProperty(0);

  /**
   * Create a new single Game Block (square).
   *
   * @param x the column the block exists in
   * @param y the row the block exists in
   * @param size the width and height of the canvas to render
   */
  public GameBlock(int x, int y, double size) {
    this.width = size;
    this.height = size;
    this.x = x;
    this.y = y;

    // A canvas needs a fixed width and height
    setWidth(width);
    setHeight(height);

    // Do an initial paint
    paint();

    // When the value property is updated, call the internal updateValue method
    value.addListener(this::updateValue);
  }

  /**
   * When the value of this block is updated repaint it.
   *
   * @param observable what was updated
   * @param oldValue the old value
   * @param newValue the new value
   */
  private void updateValue(
      ObservableValue<? extends Number> observable, Number oldValue, Number newValue) {
    paint();
  }

  /** Handle painting of the block canvas. */
  private void paint() {

    // If the block is empty, paint as empty
    if (value.get() == 0) {
      paintEmpty();
    } else {
      // If the block is not empty, paint with the colour represented by the value
      paintColor(Constants.COLOURS[value.get()]);
    }

    // Check if the block has a dot on top
    if (dot) {
      paintDot();
    }

    // Check if the block has a shadow on top
    if (hovered) {
      // logger.info("Hovered!");
      paintHovered();
    }
  }

  /** Changing block color when hovered. */
  private void paintHovered() {
    GraphicsContext gc = this.getGraphicsContext2D();
    gc.setFill(hoverColor);
    gc.fillRect(0, 0, width, height);
  }

  /** Paint this canvas empty. */
  private void paintEmpty() {
    var gc = getGraphicsContext2D();

    // Clear
    gc.clearRect(0, 0, width, height);

    // Set up the empty block gradient
    var start = new Stop(0, Color.color(0, 0, 0, 0.3));
    var end = new Stop(1, Color.color(0, 0, 0, 0.5));
    var gradient = new LinearGradient(0, 0, 1, 1, true, CycleMethod.REFLECT, start, end);

    // Fill
    gc.setFill(gradient);
    gc.fillRect(0, 0, width, height);

    // Border
    gc.setStroke(Color.color(1, 1, 1, 0.5));
    gc.strokeRect(0, 0, width, height);
  }

  /**
   * Paint this canvas with the given color add style the game block.
   *
   * @param color the color to paint
   */
  private void paintColor(Paint color) {
    var gc = getGraphicsContext2D();

    // Clear
    gc.clearRect(0, 0, width, height);

    // Fill
    gc.setFill(color);
    gc.fillRect(0, 0, width, height);

    // Make the lighter side
    gc.setFill(Color.color(1, 1, 1, 0.3));
    gc.fillPolygon(new double[] {0, width, 0}, new double[] {0, 0, height}, 3);

    // Adding dark accent
    gc.setFill(Color.color(1, 1, 1, 0.4));
    gc.fillRect(0, 0, width, 3);
    gc.setFill(Color.color(1, 1, 1, 0.4));
    gc.fillRect(0, 0, 3, height);

    // Adding light accent
    gc.setFill(Color.color(0, 0, 0, 0.4));
    gc.fillRect(width - 3, 0, width, height);
    gc.setFill(Color.color(0, 0, 0, 0.4));
    gc.fillRect(0, height - 3, width, height);

    // Border
    gc.setStroke(Color.color(0, 0, 0, 0.6));
    gc.strokeRect(0, 0, width, height);
  }

  /** Paint the grey dot on top of the block. */
  protected void paintDot() {
    GraphicsContext gc = getGraphicsContext2D();
    gc.setFill(Color.color(1, 1, 1, 0.7));
    gc.fillOval(width / 4, height / 4, width / 2, height / 2);
  }

  /**
   * Get the column of this block.
   *
   * @return column number
   */
  public int getX() {
    return x;
  }

  /**
   * Get the row of this block.
   *
   * @return row number
   */
  public int getY() {
    return y;
  }

  /**
   * Bind the value of this block to another property. Used to link the visual block to a
   * corresponding block in the Grid.
   *
   * @param input property to bind the value to
   */
  protected void bind(ObservableValue<? extends Number> input) {
    value.bind(input);
  }

  /**
   * Determine the color of the block shadow depending on the fact if the game piece can be placed
   * on the Game Board.
   *
   * @param hovered hover status of the game block
   * @param canPlace check if the game piece associated with the block can be placed
   */
  protected void setHovered(boolean hovered, boolean canPlace) {
    this.hovered = hovered;

    // Grey-ish color if the block can be placed or red-ish otherwise
    if (canPlace) {
      hoverColor = Color.color(1, 1, 1, 0.5);
    } else {
      hoverColor = Color.color(1, 0.2, 0.2, 0.5);
    }

    // Repaint the block
    paint();
  }

  /** Put a dot on the block and repaint the block. */
  protected void setDot() {
    dot = true;
    paint();
  }

  /** Set a fade out effect on the game block. */
  protected void fadeOut() {
    timer =
        new AnimationTimer() {
          double opacity = 1;

          @Override
          public void handle(long l) {
            paintEmpty();
            opacity -= 0.03;
            if (opacity <= 0.2) {
              this.stop();
              timer = null;
            } else {
              var gc = getGraphicsContext2D();
              gc.setFill(Color.color(0, 1, 0, opacity));
              gc.fillRect(0, 0, width, height);
            }
          }
        };
    timer.start();
  }
}
