package uk.ac.soton.comp1206.util;

import javafx.scene.paint.Color;

/** Class that holds all the main game constants. */
public final class Constants {

  /** The total number of pieces in this game. */
  public static final int PIECES = 15;

  /** Set the number of scores to be shown. */
  public static final int TOP_SCORE_COUNT = 10;

  /** Background music volume. */
  public static final double BACKGROUND_VOLUME = 0.8;

  /** Piece boards in instructions constants. */
  public static final int PIECE_PER_COLUMN = 8;
  public static final int PIECE_PADDING_OFFSET = 50;
  public static int PIECE_RELATIVE_SIZE;

  /** Time constants for the game loop. */
  public static final int MAXIMUM_TIME = 12000;
  public static final int MINIMUM_TIME = 2500;
  public static final int TIME_DECREASE_STEP = 500;

  /** Server side constants. */
  public static final int PIECE_REQUEST_SIZE = 10;
  public static final int REFRESH_INTERVAL = 2000;

  /** Score list wait time before exit in milliseconds. */
  public static final int WAIT_TIME = 14000;

  /** The set of colours for different pieces. */
  public static final Color[] COLOURS = {
    Color.TRANSPARENT,
    Color.DEEPPINK,
    Color.RED,
    Color.ORANGE,
    Color.YELLOW,
    Color.YELLOWGREEN,
    Color.LIME,
    Color.GREEN,
    Color.DARKGREEN,
    Color.DARKTURQUOISE,
    Color.DEEPSKYBLUE,
    Color.AQUA,
    Color.AQUAMARINE,
    Color.BLUE,
    Color.MEDIUMPURPLE,
    Color.PURPLE
  };
}
