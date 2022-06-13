package uk.ac.soton.comp1206.event;

import uk.ac.soton.comp1206.game.GamePiece;

/** Next Piece Listener listens for when a new piece is generated. */
public interface NextPieceListener {

  /**
   * Handle the incoming game piece.
   *
   * @param piece the piece to add to the game
   */
  void nextPiece(GamePiece piece);
}
