package uk.ac.soton.comp1206.scene.build;

import javafx.geometry.HPos;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.layout.*;
import javafx.scene.shape.Rectangle;
import javafx.scene.text.Text;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.soton.comp1206.component.GameBlock;
import uk.ac.soton.comp1206.component.GameBoard;
import uk.ac.soton.comp1206.component.PieceBoard;
import uk.ac.soton.comp1206.scene.base.BaseGameScene;
import uk.ac.soton.comp1206.ui.GameWindow;

/**
 * Builder for the Challenge Scene extending the base game scene.
 */
public abstract class ChallengeSceneBuilder extends BaseGameScene {

  private static final Logger logger = LogManager.getLogger(ChallengeSceneBuilder.class);

  /** Smaller piece boards. */
  public static PieceBoard currentPiece;
  public static PieceBoard nextPiece;

  /** UI elements. */
  protected VBox sideBar;

  protected GridPane topBar;
  protected BorderPane borderPane;
  protected Text multiplierField;

  /**
   * Create a new scene, passing in the GameWindow the scene will be displayed in.
   *
   * @param gameWindow the game window
   */
  public ChallengeSceneBuilder(GameWindow gameWindow) {
    super(gameWindow);
  }

  /** Build the Challenge window. */
  @Override
  public void build() {
    logger.info("Building " + this.getClass().getName());

    // Setup and start a new game
    setupGame();

    // Setting up the pane
    var pane = setupStackPane("challenge-background");

    // Layout generation
    generateBorder(pane);
    generateSideBar();
    generateTopBar();

    // UI elements generation
    generateScoreBox();
    generateMultiplierBox();
    generateTitle("Challenge Mode");
    generateLivesBox();
    generateHighscore();
    generateLevel();
    generateSideBoards();
    generateTimerBar();

    // Handle block on game board being clicked
    board.setOnBlockClick(this::blockClicked);
    board.setOnRightClick(this::rotateBlock);
  }

  /** Generate the timer bar UI element. */
  protected void generateTimerBar() {
    timerStack = new StackPane();
    borderPane.setBottom(timerStack);
    timer = new Rectangle();
    timer.setHeight(20);
    BorderPane.setMargin(timerStack, new Insets(5, 5, 5, 5));
    timerStack.getChildren().add(timer);
    StackPane.setAlignment(timer, Pos.CENTER_LEFT);
  }

  /** Generate the level related UI elements */
  protected void generateLevel() {
    Text levelLabel = new Text("Level");
    levelLabel.getStyleClass().add("heading");
    sideBar.getChildren().add(levelLabel);

    Text levelField = new Text("0");
    levelField.getStyleClass().add("level");
    sideBar.getChildren().add(levelField);
    levelField.textProperty().bind(game.levelProperty().asString());
  }

  /** Generate the highscore related UI elements */
  protected void generateHighscore() {
    Text highscoreLabel = new Text("High Score");
    highscoreLabel.getStyleClass().add("heading");
    sideBar.getChildren().add(highscoreLabel);

    Text highscoreField = new Text("0");
    highscoreField.getStyleClass().add("hiscore");
    sideBar.getChildren().add(highscoreField);
    highscoreField.textProperty().bind(highscore.asString());
  }

  /** Generate the player lives related UI elements */
  protected void generateLivesBox() {
    VBox livesBox = new VBox();
    livesBox.setAlignment(Pos.CENTER);
    Text livesLabel = new Text("Lives");
    livesLabel.getStyleClass().add("heading");
    livesBox.getChildren().add(livesLabel);

    Text livesField = new Text("0");
    livesField.getStyleClass().add("lives");
    livesField.textProperty().bind(game.livesProperty().asString());
    livesBox.getChildren().add(livesField);
    topBar.add(livesBox, 2, 0);
  }

  /** Generate the window title related UI elements */
  protected void generateTitle(String text) {
    Text title = new Text(text);
    title.getStyleClass().add("title");
    topBar.add(title, 1, 0);

    GridPane.setFillWidth(title, true);
    GridPane.setHgrow(title, Priority.ALWAYS);
    GridPane.setHalignment(title, HPos.CENTER);
  }

  /** Generate the player score related UI elements */
  protected void generateScoreBox() {
    VBox scoreBox = new VBox();
    scoreBox.setPrefWidth(140);
    scoreBox.setAlignment(Pos.CENTER);
    Text scoreLabel = new Text("Score");
    scoreLabel.getStyleClass().add("heading");
    scoreBox.getChildren().add(scoreLabel);

    Text scoreField = new Text("0");
    scoreField.getStyleClass().add("score");
    scoreField.textProperty().bind(score.asString());
    scoreBox.getChildren().add(scoreField);
    topBar.add(scoreBox, 0, 0);
  }

  /** Generate the multiplier box element. */
  protected void generateMultiplierBox() {
    VBox multiplierBox = new VBox();
    multiplierBox.setPrefWidth(140);
    multiplierBox.setAlignment(Pos.CENTER);

    multiplierField = new Text("X1");
    multiplierField.getStyleClass().add("score");
    multiplierField.getStyleClass().add(mult.get());
    multiplierField.textProperty().bind(mult);
    multiplierBox.getChildren().add(multiplierField);
    topBar.add(multiplierBox, 0, 1);
  }

  /** Generate the top bar in the layout that will contain the score, lives and the window title */
  protected void generateTopBar() {
    topBar = new GridPane();
    topBar.setPadding(new Insets(10, 10, 10, 10));
    borderPane.setTop(topBar);
  }

  /**
   * Generate the sidebar in the layout that will contain the highscore, level and next game piece.
   */
  protected void generateSideBar() {
    sideBar = new VBox();
    sideBar.setAlignment(Pos.CENTER);
    sideBar.setSpacing(6);
    sideBar.setPadding(new Insets(5, 5, 5, 5));
    borderPane.setRight(sideBar);
  }

  /**
   * Generate the borderpane with the main grid, top bar and sidebar.
   *
   * @param pane the stack pane with all the UI elements
   */
  protected void generateBorder(StackPane pane) {
    borderPane = new BorderPane();
    pane.getChildren().add(borderPane);
    board = new GameBoard(game.getGrid(), gameWindow.getWidth() / 2.0, gameWindow.getWidth() / 2.0);
    board.setGame(game);
    borderPane.setCenter(board);
  }

  /** Generate the next piece side board related UI elements. */
  protected void generateSideBoards() {
    Text nextPieceLabel = new Text("Current Piece");
    nextPieceLabel.getStyleClass().add("heading");
    sideBar.getChildren().add(nextPieceLabel);

    // Generate the board for the current piece
    currentPiece = new PieceBoard(3, 3, gameWindow.getWidth() / 6.0, gameWindow.getWidth() / 6.0);
    currentPiece.middleDot();
    currentPiece.setOnBlockClick(this::rotateBlock);
    sideBar.getChildren().add(currentPiece);

    // Generate the board for the next piece
    nextPiece = new PieceBoard(3, 3, gameWindow.getWidth() / 10.0, gameWindow.getWidth() / 10.0);
    nextPiece.setPadding(new Insets(20, 0, 0, 0));
    nextPiece.setOnBlockClick(this::swapBlock);
    sideBar.getChildren().add(nextPiece);
  }

  public abstract void setupGame();

  protected abstract void swapBlock(GameBlock gameBlock);

  protected abstract void rotateBlock(GameBlock gameBlock);
}
