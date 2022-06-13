package uk.ac.soton.comp1206.scene.base;

import javafx.geometry.Insets;
import javafx.scene.Scene;
import javafx.scene.layout.*;
import javafx.scene.paint.Color;

import uk.ac.soton.comp1206.game.Game;
import uk.ac.soton.comp1206.ui.GamePane;
import uk.ac.soton.comp1206.ui.GameWindow;

import java.util.Objects;

/**
 * A Base Scene used in the game. Handles common functionality between all scenes.
 */
public abstract class BaseScene {

    /**
     * The game window of the scene.
     */
    public static GameWindow gameWindow = null;

    /**
     * UI and backend objects.
     */
    protected GamePane root;
    protected Scene scene;
    protected Game game;

    /**
     * Create a new scene, passing in the GameWindow the scene will be displayed in
     * @param gameWindow the game window
     */
    public BaseScene(GameWindow gameWindow) {
        BaseScene.gameWindow = gameWindow;
    }

    /**
     * Initialise this scene. Called after creation
     */
    public abstract void initialise();

    /**
     * Build the layout of the scene
     */
    public abstract void build();

    /**
     * Create a new JavaFX scene using the root contained within this scene
     * @return JavaFX scene
     */
    public Scene setScene() {
        var previous = gameWindow.getScene();
        Scene scene = new Scene(root, previous.getWidth(), previous.getHeight(), Color.BLACK);
        scene.getStylesheets().add(Objects.requireNonNull(getClass().getResource("/style/game.css")).toExternalForm());
        this.scene = scene;
        return scene;
    }

    /**
     * Setting up a standard BorderPane for the scene
     *
     * @return standard BorderPane object for scene
     */
    public BorderPane setupPane() {
        root = new GamePane(gameWindow.getWidth(),gameWindow.getHeight());

        var pane = new StackPane();
        pane.setMaxWidth(gameWindow.getWidth());
        pane.setMaxHeight(gameWindow.getHeight());
        pane.getStyleClass().add("menu-background");
        root.getChildren().add(pane);

        var borderPane = new BorderPane();
        pane.getChildren().add(borderPane);
        return borderPane;
    }

    /**
     * Generate the main pane for the scene
     *
     * @return stack pane object to add the layout and UI elements to
     */
    public StackPane setupStackPane(String background) {
        root = new GamePane(gameWindow.getWidth(), gameWindow.getHeight());
        var pane = new StackPane();
        pane.setMaxWidth(gameWindow.getWidth());
        pane.setMaxHeight(gameWindow.getHeight());
        pane.setBackground(
          new Background(new BackgroundFill(Color.BLACK, CornerRadii.EMPTY, Insets.EMPTY)));
        pane.getStyleClass().add(background);
        root.getChildren().add(pane);
        return pane;
    }
}
