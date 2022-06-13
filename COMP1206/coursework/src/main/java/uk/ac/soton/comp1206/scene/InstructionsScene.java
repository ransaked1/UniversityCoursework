package uk.ac.soton.comp1206.scene;

import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.image.ImageView;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.VBox;
import javafx.scene.text.Text;
import javafx.scene.text.TextAlignment;
import javafx.scene.text.TextFlow;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.soton.comp1206.component.PieceBoard;
import uk.ac.soton.comp1206.game.GamePiece;
import uk.ac.soton.comp1206.scene.base.BaseScene;
import uk.ac.soton.comp1206.ui.GameWindow;
import uk.ac.soton.comp1206.util.Constants;

import java.util.Objects;

/** The Instructions scene. Holds the UI with dynamically generated game pieces. */
public class InstructionsScene extends BaseScene {

  private static final Logger logger = LogManager.getLogger(InstructionsScene.class);
  private BorderPane mainPane;

  public InstructionsScene(GameWindow gameWindow) {
    super(gameWindow);
    // Setting the size of piece boards in instructions menu relative to the window size
    Constants.PIECE_RELATIVE_SIZE = gameWindow.getWidth() / 12;
    logger.info("Creating Instructions Scene");
  }

  /** Initializing the scene with listener that exits on any key press. */
  @Override
  public void initialise() {
    scene.setOnKeyPressed(
        (e) -> {
          if (e.getCode().equals(KeyCode.ESCAPE)) gameWindow.startMenu();
        });
  }

  /** Building the scene. */
  @Override
  public void build() {
    // Generating pane and layout
    mainPane = setupPane();
    VBox vBox = generateLayout();

    // Generating and adding UI elements
    generateTitle(vBox);
    generateText(vBox);
    generateImage(vBox);
    generateGamePieces(vBox);
  }

  /**
   * Generating all the game pieces in the game and adding them to the instruction's vbox.
   *
   * @param vBox UI layout element to add the elements to
   */
  private void generateGamePieces(VBox vBox) {
    Text pieces = new Text("Game Pieces");
    pieces.getStyleClass().add("heading");
    vBox.getChildren().add(pieces);

    // Grid pane for the pieces
    GridPane gridPane = new GridPane();
    gridPane.setVgap(12);
    gridPane.setHgap(12);
    vBox.getChildren().add(gridPane);

    double totalPiecesSize = Constants.PIECE_RELATIVE_SIZE * Constants.PIECE_PER_COLUMN;
    double padding = (gameWindow.getWidth() - totalPiecesSize - Constants.PIECE_PADDING_OFFSET) / 2;
    gridPane.setPadding(new Insets(0, padding, 0, padding));

    // Generate the game pieces each on their own boards
    int col = 0;
    int row = 0;
    for (int i = 0; i < Constants.PIECES; i++) {
      GamePiece piece = GamePiece.createPiece(i);
      PieceBoard pieceBoard =
          new PieceBoard(3, 3, Constants.PIECE_RELATIVE_SIZE, Constants.PIECE_RELATIVE_SIZE);
      pieceBoard.setPiece(piece);
      gridPane.add(pieceBoard, col, row);
      col++;
      if (col == Constants.PIECE_PER_COLUMN) {
        col = 0;
        row++;
      }
    }
  }

  /**
   * Generating the instructions image.
   *
   * @param vBox UI vbox to add to
   */
  private void generateImage(VBox vBox) {
    ImageView instructionImage =
        new ImageView(
            Objects.requireNonNull(getClass().getResource("/images/Instructions.png"))
                .toExternalForm());
    instructionImage.setFitWidth((double) gameWindow.getWidth() / 1.5);
    instructionImage.setPreserveRatio(true);
    vBox.getChildren().add(instructionImage);
  }

  /**
   * Generate the top text explaining the game.
   *
   * @param vBox UI vbox to add to
   */
  private void generateText(VBox vBox) {
    var instructionText =
        new Text(
            "TetrECS is a fast-paced gravity-free block placement game where you must survive by clearing rows through careful placement of blocks before the time runs out. You got 3 lives so make them count!");
    TextFlow instructionFlow = new TextFlow(instructionText);
    instructionText.getStyleClass().add("instructions");
    instructionText.setTextAlignment(TextAlignment.CENTER);
    instructionFlow.setTextAlignment(TextAlignment.CENTER);
    vBox.getChildren().add(instructionFlow);
  }

  /**
   * Generate the window title.
   *
   * @param vBox UI vbox to add to
   */
  private void generateTitle(VBox vBox) {
    Text title = new Text("Instructions");
    title.getStyleClass().add("heading");
    vBox.getChildren().add(title);
  }

  /**
   * Generating the vbox that will contain all the scene's elements.
   *
   * @return the vbox object
   */
  private VBox generateLayout() {
    VBox vBox = new VBox();
    BorderPane.setAlignment(vBox, Pos.CENTER);
    vBox.setAlignment(Pos.TOP_CENTER);
    mainPane.setCenter(vBox);
    return vBox;
  }
}
