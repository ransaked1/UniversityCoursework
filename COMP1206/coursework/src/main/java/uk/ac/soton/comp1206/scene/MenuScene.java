package uk.ac.soton.comp1206.scene;

import javafx.animation.TranslateTransition;
import javafx.geometry.Pos;
import javafx.scene.image.ImageView;
import javafx.scene.layout.*;
import javafx.util.Duration;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.soton.comp1206.App;
import uk.ac.soton.comp1206.scene.base.BaseScene;
import uk.ac.soton.comp1206.ui.GameWindow;
import uk.ac.soton.comp1206.ui.MenuWindow;
import uk.ac.soton.comp1206.util.Multimedia;

/** The main menu of the game. Provides a gateway to the rest of the game. */
@SuppressWarnings("EmptyMethod")
public class MenuScene extends BaseScene {

  private static final Logger logger = LogManager.getLogger(MenuScene.class);
  private BorderPane mainPane;

  /**
   * Create a new menu scene
   *
   * @param gameWindow the Game Window this will be displayed in
   */
  public MenuScene(GameWindow gameWindow) {
    super(gameWindow);
    logger.info("Creating Menu Scene");
  }

  /** Build the menu layout */
  @Override
  public void build() {
    logger.info("Building " + this.getClass().getName());

    // Setup scene pane
    mainPane = setupPane();

    // Generate menu UI
    generateMenuImage();
    generateMenuWindow();
  }

  /** Generate menu buttons and bind them to other scenes */
  private void generateMenuWindow() {
    MenuWindow menuWindow = new MenuWindow();
    BorderPane.setAlignment(menuWindow, Pos.CENTER);

    menuWindow.add("Solo Game", gameWindow::startChallenge, false);
    menuWindow.add("Online Game", gameWindow::startMultiplayerLobby, true);
    menuWindow.add("Leaderboard", gameWindow::startLeaderboard, false);
    menuWindow.add("How to Play", gameWindow::startInstructions, false);
    menuWindow.add("", this::dummy, false); // Dummy button to add a space before Exit
    menuWindow.add("Exit", () -> App.getInstance().shutdown(), false);

    mainPane.setBottom(menuWindow);
  }

  /** Dummy function for the dummy menu button */
  private void dummy() {}

  /** Generate the main menu image and make it rotate */
  private void generateMenuImage() {
    ImageView menuImage = new ImageView(Multimedia.getImage("TetrECS.png"));
    menuImage.setFitWidth(gameWindow.getWidth() / 1.2);
    menuImage.setPreserveRatio(true);
    mainPane.setCenter(menuImage);

    var translate = new TranslateTransition(new Duration(2300), menuImage);
    translate.setCycleCount(-1);
    translate.setByY(30);
    translate.setAutoReverse(true);
    translate.play();
  }

  /** Initialise the menu */
  @Override
  public void initialise() {
    Multimedia.startBackgroundMusic("menu.mp3", true);
  }
}
