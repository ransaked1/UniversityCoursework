package uk.ac.soton.comp1206.scene.build;

import javafx.beans.property.*;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.geometry.HPos;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.image.ImageView;
import javafx.scene.layout.*;
import javafx.scene.text.Text;
import javafx.scene.text.TextAlignment;
import javafx.util.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.soton.comp1206.ui.ScoreField;
import uk.ac.soton.comp1206.game.Game;
import uk.ac.soton.comp1206.game.GamePiece;
import uk.ac.soton.comp1206.network.Communicator;
import uk.ac.soton.comp1206.scene.base.BaseScene;
import uk.ac.soton.comp1206.ui.GamePane;
import uk.ac.soton.comp1206.ui.GameWindow;
import uk.ac.soton.comp1206.util.Multimedia;
import uk.ac.soton.comp1206.util.Storage;

import java.util.ArrayList;

/**
 * Builder for the Score Scene holding UI elements.
 */
public abstract class ScoreSceneBuilder extends BaseScene {

  private static final Logger logger = LogManager.getLogger(ScoreSceneBuilder.class);

  /** UI primitives. */
  protected VBox scoreBox;

  /** Fields for the scores */
  protected ScoreField highScoreColumn1;
  protected ScoreField highScoreColumn2;

  /** Lists for extracting the local and remote scores */
  protected ObservableList<Pair<String, Integer>> localScores;
  protected ObservableList<Pair<String, Integer>> remoteScores;
  protected final ArrayList<Pair<String, Integer>> remoteScoresList = new ArrayList<>();

  /** Bindable properties */
  protected final StringProperty localName = new SimpleStringProperty("");
  protected final BooleanProperty showScores = new SimpleBooleanProperty(false);

  /** Communicator object for online scores */
  protected final Communicator communicator;

  /**
   * Build the score scene after the game is finished.
   *
   * @param gameWindow the game window that will hold the scene
   * @param game the game with the scores and other data
   */
  public ScoreSceneBuilder(GameWindow gameWindow, Game game) {
    super(gameWindow);
    logger.info("Creating Score Scene");
    communicator = gameWindow.getCommunicator();
    this.game = game;
  }

  /**
   * Build the score scene for the leaderboard with a dummy game.
   *
   * @param gameWindow the game window that will hold the scene
   */
  public ScoreSceneBuilder(GameWindow gameWindow) {
    super(gameWindow);
    logger.info("Creating Leaderboard Scene");

    communicator = gameWindow.getCommunicator();

    // Dummy game object
    game = new Game() {
      @Override
      public GamePiece spawnPiece() {
        return null;
      }
    };
  }

  /** Build the Score window. */
  @Override
  public void build() {
    this.root = new GamePane(gameWindow.getWidth(), gameWindow.getHeight());

    // Generating layout elements
    StackPane scorePane = setupStackPane("score-background");
    generateBorderPane(scorePane);

    // Generating UI elements
    generateTopImage();
    generateTitle();

    // Grid layout (causes a bug when moved up)
    GridPane scoreGrid = generateScoreGrid();

    // Generating score grid contents
    generateLocalScoreText(scoreGrid);
    generateRemoteScoreText(scoreGrid);
    generateAndBindLocalScores(scoreGrid);
    generateAndBindRemoteScores(scoreGrid);
  }

  /**
   * Initializing and binding remote scores on the right side of the grid.
   *
   * @param scoreGrid the grid pane to add the scores to
   */
  private void generateAndBindRemoteScores(GridPane scoreGrid) {
    highScoreColumn2 = new ScoreField();
    Text text = new Text("");
    highScoreColumn2.getChildren().add(text);
    GridPane.setHalignment(highScoreColumn2, HPos.CENTER);
    scoreGrid.add(highScoreColumn2, 1, 1);

    remoteScores = FXCollections.observableArrayList(this.remoteScoresList);
    SimpleListProperty<Pair<String, Integer>> wrapper = new SimpleListProperty<>(remoteScores);
    highScoreColumn2.scoreProperty().bind(wrapper);
    highScoreColumn2.nameProperty().bind(localName);
  }

  /**
   * Initializing and binding local scores on the left side of the grid.
   *
   * @param scoreGrid the grid pane to add the scores to
   */
  private void generateAndBindLocalScores(GridPane scoreGrid) {
    highScoreColumn1 = new ScoreField();
    Text text = new Text("");
    highScoreColumn1.getChildren().add(text);
    GridPane.setHalignment(highScoreColumn1, HPos.CENTER);
    scoreGrid.add(highScoreColumn1, 0, 1);

    localScores.sort((a, b) -> b.getValue().compareTo(a.getValue()));
    SimpleListProperty<Pair<String, Integer>> wrapper = new SimpleListProperty<>(localScores);
    highScoreColumn1.scoreProperty().bind(wrapper);
    highScoreColumn1.nameProperty().bind(localName);
  }

  /**
   * Generate the text for the remote scores.
   *
   * @param scoreGrid the grid to add the text to
   */
  private void generateRemoteScoreText(GridPane scoreGrid) {
    Text remoteScoresLabel = new Text("Online Scores");
    GridPane.setHalignment(remoteScoresLabel, HPos.CENTER);
    remoteScoresLabel.setTextAlignment(TextAlignment.CENTER);
    remoteScoresLabel.getStyleClass().add("heading");
    scoreGrid.add(remoteScoresLabel, 1, 0);
  }

  /**
   * Generate the text for the local scores.
   *
   * @param scoreGrid the grid to add the text to
   */
  private void generateLocalScoreText(GridPane scoreGrid) {
    Text localScoresLabel = new Text("Local Scores");
    GridPane.setHalignment(localScoresLabel, HPos.CENTER);
    localScoresLabel.setTextAlignment(TextAlignment.CENTER);
    localScoresLabel.getStyleClass().add("heading");

    if (game.getScores().isEmpty()) {
      localScores = FXCollections.observableArrayList(Storage.loadScores());
    } else {
      localScores = FXCollections.observableArrayList(game.getScores());
      localScoresLabel.setText("Your score");
    }

    scoreGrid.add(localScoresLabel, 0, 0);
  }

  /**
   * Generate the layout of the score columns.
   *
   * @return the grid pane that will hold the scores
   */
  private GridPane generateScoreGrid() {
    GridPane scoreGrid = new GridPane();
    scoreGrid.setHgap(120);
    scoreGrid.setVgap(10);
    scoreGrid.visibleProperty().bind(showScores);
    scoreGrid.setAlignment(Pos.CENTER);
    scoreBox.getChildren().add(scoreGrid);
    return scoreGrid;
  }

  /**
   * Generate the title text of the scene.
   */
  private void generateTitle() {
    Text gameOverText = new Text("Leaderboard");
    gameOverText.setTextAlignment(TextAlignment.CENTER);
    VBox.setVgrow(gameOverText, Priority.ALWAYS);
    gameOverText.getStyleClass().add("title");
    scoreBox.getChildren().add(gameOverText);
  }

  /**
   * Generate the image at the top of the scene.
   */
  private void generateTopImage() {
    ImageView image = new ImageView(Multimedia.getImage("TetrECS.png"));
    image.setFitWidth((double) gameWindow.getWidth() * 0.7);
    image.setPreserveRatio(true);
    scoreBox.getChildren().add(image);
  }

  /**
   * Generate the pane that will hold the score boxes with scores.
   *
   * @param scorePane the stack pane that will hold the columns
   */
  private void generateBorderPane(StackPane scorePane) {
    BorderPane borderPane = new BorderPane();
    scorePane.getChildren().add(borderPane);
    scoreBox = new VBox();
    scoreBox.setAlignment(Pos.TOP_CENTER);
    scoreBox.setPadding(new Insets(10));
    scoreBox.setSpacing(20);
    borderPane.setCenter(scoreBox);
  }
}
