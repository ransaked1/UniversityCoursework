package uk.ac.soton.comp1206.event;

import java.util.HashSet;

import uk.ac.soton.comp1206.component.GameBlockCoordinate;

/** Line Cleared Listener listens for when a line is completed and has to be cleared. */
public interface LineClearedListener {

  /**
   * Handle the clearing of the blocks in the lines.
   *
   * @param coordinates the coordinates of the blocks to clear
   */
  void lineCleared(HashSet<GameBlockCoordinate> coordinates);
}
