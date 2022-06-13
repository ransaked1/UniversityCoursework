package uk.ac.soton.comp1206.scene;

import javafx.animation.FadeTransition;
import javafx.animation.PauseTransition;
import javafx.animation.SequentialTransition;
import javafx.scene.image.ImageView;
import javafx.scene.layout.StackPane;
import javafx.util.Duration;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.soton.comp1206.scene.base.BaseScene;
import uk.ac.soton.comp1206.ui.GamePane;
import uk.ac.soton.comp1206.ui.GameWindow;
import uk.ac.soton.comp1206.util.Multimedia;

/**
 * The Start Scene that flashes at the start of the game.
 */
public class StartScene extends BaseScene {

  private static final Logger logger = LogManager.getLogger(StartScene.class);

  /** Sequential transition for the fade animation. */
  private SequentialTransition sequence;

  /**
   * Create a new scene, passing in the GameWindow the scene will be displayed in.
   *
   * @param gameWindow the game window
   */
  public StartScene(GameWindow gameWindow) {
    super(gameWindow);
    logger.info("Creating Intro Scene");
    Multimedia.playAudio("intro.mp3");
  }

  /** Initializing the scene. Can be skipped by pressing any button. */
  public void initialise() {
    scene.setOnKeyPressed(
        (e) -> {
          Multimedia.stopAll();
          sequence.stop();
          gameWindow.startMenu();
        });
  }

  /**
   * Build the scene UI.
   */
  public void build() {
    logger.info("Building " + this.getClass().getName());

    StackPane introPane = generateLayout();
    ImageView logo = generateGameLogo(introPane);
    animateLogo(logo);
  }

  /**
   * Animate the logo with a fade transition.
   *
   * @param logo the image to animate
   */
  private void animateLogo(ImageView logo) {
    FadeTransition fadeIn = new FadeTransition(new Duration(2000), logo);
    PauseTransition pause = new PauseTransition(new Duration(2000));
    FadeTransition fadeOut = new FadeTransition(new Duration(1000), logo);
    fadeOut.setToValue(0);
    fadeIn.setToValue(1);
    sequence = new SequentialTransition(fadeIn, pause, fadeOut);
    sequence.play();
    sequence.setOnFinished((e) -> gameWindow.startMenu());
  }

  /**
   * Set up the image for the scene.
   *
   * @param introPane the stack pane to add to
   * @return the resulting image view
   */
  private ImageView generateGameLogo(StackPane introPane) {
    ImageView logo = new ImageView(Multimedia.getImage("ECSGames.png"));
    logo.setFitWidth(gameWindow.getWidth() / 2.0);
    logo.setPreserveRatio(true);
    logo.setOpacity(0);
    introPane.getChildren().add(logo);
    root.getChildren().add(introPane);
    return logo;
  }

  /**
   * Generate the layout of the intro scene.
   *
   * @return the stack pane that will contain the UI elements
   */
  private StackPane generateLayout() {
    root = new GamePane(gameWindow.getWidth(), gameWindow.getHeight());
    StackPane introPane = new StackPane();
    introPane.setMaxWidth(gameWindow.getWidth());
    introPane.setMaxHeight(gameWindow.getHeight());
    introPane.getStyleClass().add("intro");
    return introPane;
  }
}
