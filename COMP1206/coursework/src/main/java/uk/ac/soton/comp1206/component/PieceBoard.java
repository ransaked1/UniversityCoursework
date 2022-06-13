package uk.ac.soton.comp1206.component;

import uk.ac.soton.comp1206.game.GamePiece;

/** GameBoard extension for the side boards that display the current and next piece. */
public class PieceBoard extends GameBoard {

  public PieceBoard(int cols, int rows, double width, double height) {
    super(cols, rows, width, height);
  }

  /**
   * Set the piece for the board to display.
   *
   * @param piece the game piece to display inside
   */
  public void setPiece(GamePiece piece) {
    grid.clear();
    grid.playPiece(piece, 1, 1);
  }

  /** Add the middle dot to the board (used for the current piece). */
  public void middleDot() {
    double midX = Math.ceil((double) getRows() / 2) - 1;
    double midY = Math.ceil((double) getCols() / 2) - 1;
    blocks[(int) midX][(int) midY].setDot();
  }
}
