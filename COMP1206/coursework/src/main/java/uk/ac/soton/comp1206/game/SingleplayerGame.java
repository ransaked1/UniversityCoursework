package uk.ac.soton.comp1206.game;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.soton.comp1206.util.Constants;

import java.util.concurrent.ThreadLocalRandom;

/** A single player game orchestrating the base game object. */
public class SingleplayerGame extends Game {

  private static final Logger logger = LogManager.getLogger(SingleplayerGame.class);
  private final ThreadLocalRandom tlr = ThreadLocalRandom.current();

  public SingleplayerGame(int cols, int rows) {
    super(cols, rows);
  }

  /**
   * Orchestrating piece spawning.
   *
   * @return a new random game piece
   */
  public GamePiece spawnPiece() {
    logger.info("Spawning next piece");
    return GamePiece.createPiece(tlr.nextInt(0, Constants.PIECES));
  }
}
