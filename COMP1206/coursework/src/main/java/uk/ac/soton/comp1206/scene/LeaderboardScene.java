package uk.ac.soton.comp1206.scene;

import uk.ac.soton.comp1206.ui.GameWindow;

/**
 * Leaderboard scene. The same as the score scene but it doesn't go to the menu after 10sec and no
 * end game music is played.
 */
public class LeaderboardScene extends ScoreScene {

  public LeaderboardScene(GameWindow gameWindow) {
    super(gameWindow);
  }

  /** Reveal overwritten to not return to the menu automatically. */
  @Override
  public void reveal() {
    scene.setOnKeyPressed((e) -> returnToMenu());
    showScores.set(true);
    highScoreColumn1.reveal();
    highScoreColumn2.reveal();
  }

  /** No music is played when this scene is started. */
  @Override
  protected void playGameEndMusic() {}
}
