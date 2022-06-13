package uk.ac.soton.comp1206.event;

/** Game Over Listener is used to listen for when the game is over. */
public interface GameOverListener {

  /** Handle the cleanup after the game is finished and transition to the next window. */
  void gameOver();
}
