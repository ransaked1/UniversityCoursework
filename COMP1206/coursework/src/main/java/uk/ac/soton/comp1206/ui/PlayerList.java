package uk.ac.soton.comp1206.ui;

import javafx.scene.text.Text;
import javafx.scene.text.TextFlow;

import java.util.ArrayList;
import java.util.List;

/**
 * UI element holding the player list in the multiplayer lobby.
 */
public class PlayerList extends TextFlow {

  /** The list of players in the channel */
  private final ArrayList<String> players = new ArrayList<>();

  /**
   * Set css class on initialization
   */
  public PlayerList() {
    this.getStyleClass().add("playerElement");
  }

  /**
   * Adding new players to the player list.
   *
   * @param newPlayers string list of new players
   */
  public void set(List<String> newPlayers) {
    players.clear();
    players.addAll(newPlayers);
    update();
  }

  /**
   * Updating the player list.
   */
  public void update() {
    getChildren().clear();

    Text names;
    for (String player : players) {
      names = new Text(player + " ");
      getChildren().add(names);
    }
  }
}
