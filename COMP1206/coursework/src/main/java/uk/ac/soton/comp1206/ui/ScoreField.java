package uk.ac.soton.comp1206.ui;

import javafx.animation.Animation;
import javafx.animation.FadeTransition;
import javafx.animation.SequentialTransition;
import javafx.animation.Transition;
import javafx.beans.InvalidationListener;
import javafx.beans.property.ListProperty;
import javafx.beans.property.SimpleListProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.geometry.Pos;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import javafx.scene.paint.Color;
import javafx.scene.text.Text;
import javafx.scene.text.TextAlignment;
import javafx.util.Duration;
import javafx.util.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.ArrayList;

import uk.ac.soton.comp1206.util.Constants;

/** UI element that holds the players and their scores. */
public class ScoreField extends VBox {
  private static final Logger logger = LogManager.getLogger(ScoreField.class);

  /** Bindable properties. */
  private final SimpleListProperty<Pair<String, Integer>> scores = new SimpleListProperty<>();

  /** The name of the local player. */
  private final StringProperty name = new SimpleStringProperty();

  /** Array lists for scores and dead players. */
  private final ArrayList<HBox> scoreFields = new ArrayList<>();

  /** Players who lost. */
  private final ArrayList<String> deadPlayers = new ArrayList<>();

  /** Auto reveal for the score field. */
  private boolean reveal = false;

  /** Score field constructor. */
  public ScoreField() {
    this.getStyleClass().add("scorelist");
    this.setAlignment(Pos.CENTER);
    this.setSpacing(5);

    scores.addListener((InvalidationListener) (c) -> this.updateList());
    name.addListener((e) -> this.updateList());
  }

  /**
   * Set the reveal status of the score field.
   *
   * @param autoReveal the value to set
   */
  public void setReveal(boolean autoReveal) {
    this.reveal = autoReveal;
  }

  /** Reveal each score with a fade transition. */
  public void reveal() {
    ArrayList<Transition> transitions = new ArrayList<>();
    for (HBox scoreBox : scoreFields) {
      FadeTransition fade = new FadeTransition(new Duration(100), scoreBox);
      fade.setFromValue(0);
      fade.setToValue(1);
      transitions.add(fade);
    }

    SequentialTransition transition =
        new SequentialTransition(transitions.toArray(Animation[]::new));
    transition.play();
  }

  /**
   * Getter for the score pairs.
   *
   * @return the list of score pairs
   */
  public ListProperty<Pair<String, Integer>> scoreProperty() {
    return scores;
  }

  /**
   * Getter for the name of the player.
   *
   * @return the player name property
   */
  public StringProperty nameProperty() {
    return name;
  }

  /**
   * Add a player to the dead list of players.
   *
   * @param player the player name to add to the list
   */
  public void kill(String player) {
    deadPlayers.add(player);
  }

  /** Update the score list. */
  private void updateList() {
    logger.info("Updating score list with {} scores", scores.size());
    scoreFields.clear();
    getChildren().clear();
    int counter = 1;

    for (Pair<String, Integer> stringIntegerPair : scores) {
      // Exit for-loop after displaying top players
      if (counter > Constants.TOP_SCORE_COUNT) break;
      counter++;

      // Generating a player and score field
      generatePlayerScorePair(stringIntegerPair, Constants.COLOURS[counter]);
    }

    if (reveal) {
      reveal();
    }
  }

  /**
   * Generate each pair of name and score for each player by parsing data pairs.
   *
   * @param stringIntegerPair the pair containing the data
   * @param color the color for the score field
   */
  private void generatePlayerScorePair(Pair<String, Integer> stringIntegerPair, Color color) {
    // HBox that will contain the name and score
    HBox scoreBox = generateScoreBox();

    // Generating the player name text
    Text playerName = stylePlayerName(stringIntegerPair);
    playerName.setTextAlignment(TextAlignment.CENTER);
    playerName.setFill(color);
    HBox.setHgrow(playerName, Priority.ALWAYS);

    // Generating the player score text
    Text playerScore = new Text((stringIntegerPair.getValue()).toString());
    playerScore.getStyleClass().add("points");
    playerScore.setTextAlignment(TextAlignment.CENTER);
    playerScore.setFill(color);
    HBox.setHgrow(playerScore, Priority.ALWAYS);

    // Add name and score to the score field
    scoreBox.getChildren().addAll(playerName, playerScore);
    getChildren().add(scoreBox);
    scoreFields.add(scoreBox);
  }

  /**
   * Control the colors of players in the live leaderboard in multiplayer
   *
   * @param stringIntegerPair the player and score pair.
   * @return the text object for the player
   */
  private Text stylePlayerName(Pair<String, Integer> stringIntegerPair) {
    Text playerName = new Text(stringIntegerPair.getKey() + ":");
    playerName.getStyleClass().add("scorer");

    // Set the style for the score fields
    if (stringIntegerPair.getKey().equals(name.get())) {
      playerName.getStyleClass().add("myscore");
    }

    // Change the style for the players who lost
    if (deadPlayers.contains(stringIntegerPair.getKey())) {
      playerName.getStyleClass().add("deadscore");
    }
    return playerName;
  }

  /**
   * Generate the score box UI element
   *
   * @return the score box object
   */
  private HBox generateScoreBox() {
    HBox scoreBox = new HBox();
    scoreBox.setOpacity(0);
    scoreBox.getStyleClass().add("scoreitem");
    scoreBox.setAlignment(Pos.CENTER);
    scoreBox.setSpacing(10);
    return scoreBox;
  }
}
