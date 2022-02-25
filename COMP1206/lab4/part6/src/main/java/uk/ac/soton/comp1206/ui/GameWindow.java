package uk.ac.soton.comp1206.ui;

import javafx.application.Platform;
import javafx.beans.property.*;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.ProgressBar;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import javafx.scene.paint.Color;
import javafx.scene.shape.Rectangle;
import javafx.scene.text.Text;
import javafx.scene.text.TextAlignment;
import javafx.util.Pair;
import uk.ac.soton.comp1206.App;
import uk.ac.soton.comp1206.game.GameBlock;
import uk.ac.soton.comp1206.game.GameGrid;
import uk.ac.soton.comp1206.game.Grid;
import uk.ac.soton.comp1206.game.Scores;
import uk.ac.soton.comp1206.network.Communicator;

import java.util.Random;
import java.util.Timer;
import java.util.TimerTask;

public class GameWindow {

  private final Color[] colors = {
    Color.BLUE, Color.PURPLE, Color.YELLOW, Color.PINK, Color.GREEN, Color.RED
  };
  private final BooleanProperty game = new SimpleBooleanProperty(false);
  private final IntegerProperty currentColor = new SimpleIntegerProperty(1);
  private final ObservableList<Pair<String, String>> scores = FXCollections.observableArrayList();
  private final ListProperty<Pair<String, String>> scoreProperty = new SimpleListProperty<>(scores);
  private final Random random = new Random();
  private final IntegerProperty score = new SimpleIntegerProperty(0);
  private final DoubleProperty timer = new SimpleDoubleProperty(1);
  private App app;
  private Communicator communicator;
  private Scene scene;
  private Grid grid;
  private BorderPane gamePane;
  private Rectangle rec;
  private double ticks = 0;
  private Timer gameTimer;

  public GameWindow(App app, Communicator communicator) {
    this.app = app;
    this.communicator = communicator;
    this.grid = new Grid(6, 6);

    for (int x = 0; x < grid.getCols(); x++) {
      for (int y = 0; y < grid.getRows(); y++) {
        grid.set(x, y, random.nextInt(colors.length));
      }
    }

    // Create a game pane
    gamePane = new BorderPane();
    // gamePane.getStyleClass().add("game");
    gamePane.setCenter(createGameGrid());
    gamePane.setRight(createScoreBoard());
    gamePane.setTop(createTopBar());
    gamePane.setBottom(createTimer());

    this.scene = new Scene(gamePane, 800, 600);

    String css = this.getClass().getResource("/chat.css").toExternalForm();
    scene.getStylesheets().add(css);

    communicator.addListener(
        (message) -> {
          if (!message.startsWith("SCORES")) return;
          Platform.runLater(() -> this.receiveScore(message));
        });

    communicator.send("SCORES");

    gameStart();
  }

  public GameGrid createGameGrid() {
    var gameGrid = new GameGrid(colors, grid, 400, 350);

    gameGrid.addListener(this::blockClicked);
    return gameGrid;
  }

  public VBox createScoreBoard() {
    var scores = new VBox();
    scores.setPadding(new Insets(5, 5, 5, 5));
    scores.setAlignment(Pos.TOP_CENTER);
    scores.getStyleClass().add("scores");

    var currentScoreLabel = new Label("Current Score");
    var scoreText = new Text();

    currentScoreLabel.getStyleClass().add("heading");
    scoreText.setTextAlignment(TextAlignment.CENTER);
    scoreText.textProperty().bind(score.asString());
    scoreText.getStyleClass().add("myscore");
    scores.getChildren().addAll(currentScoreLabel, scoreText);

    var scoreLabel = new Label("High Scores");
    scoreLabel.getStyleClass().add("heading");
    scores.getChildren().add(scoreLabel);

    var scoreBox = new Scores();
    scores.getChildren().add(scoreBox);
    scoreBox.scoreProperty().bind(scoreProperty);
    scoreBox.usernameProperty().bind(app.usernameProperty());

    return scores;
  }

  public HBox createTopBar() {
    HBox toolbar = new HBox();
    toolbar.setPrefHeight(100);
    toolbar.setAlignment(Pos.CENTER);
    toolbar.setPadding(new Insets(6, 6, 6, 6));

    rec = new Rectangle();
    rec.setStroke(Color.BLACK);
    rec.setFill(Color.RED);
    toolbar.getChildren().add(rec);
    rec.setWidth(80);
    rec.setHeight(80);
    HBox.setHgrow(rec, Priority.ALWAYS);

    return toolbar;
  }

  public ProgressBar createTimer() {
    var timerBar = new ProgressBar(1);
    timerBar.prefWidthProperty().bind(gamePane.widthProperty());
    timerBar.progressProperty().bind(timer);
    return timerBar;
  }

  public void receiveScore(String message) {
    scores.clear();
    String[] scores = message.split("\n");

    for (String score : scores) {
      String[] parts = score.split("=");

      if (parts.length < 2) continue;

      String name = parts[0];
      String points = parts[1];
      this.scores.add(new Pair<>(name, points));
    }
  }

  public void gameStart() {
    score.set(0);
    TimerTask task =
        new TimerTask() {
          public void run() {
            loopGame();
          }
        };
    gameTimer = new Timer("Timer");
    gameTimer.schedule(task, 0, 2000);
  }

  public void blockClicked(GameBlock block) {
    var gridBlock = grid.getGridProperty(block.getX(), block.getY());
    if (currentColor.get() == gridBlock.get()) {
      score.set(score.get() + 5);
      gridBlock.set(random.nextInt(colors.length));
    } else {
      score.set(score.get() - 1);
      if (score.get() < 0) {
        score.set(0);
      }
    }
  }

  public void loopGame() {

    int randomColor = random.nextInt(colors.length);
    currentColor.set(randomColor);
    rec.setFill(colors[randomColor]);

    if (timer.get() > 0) {
      ticks++;
      double maxTicks = 30;
      timer.set((maxTicks - ticks) / maxTicks);
    } else {
      stopGame();
    }
  }

  public void stopGame() {
    Platform.runLater(
        () -> {
          gameTimer.cancel();
          showScores();
          communicator.send("SCORE " + app.getUsername() + " " + score.get());
        });
  }

  public void showScores() {

    var pane = new BorderPane();
    pane.getStyleClass().add("game");

    var scores = new VBox();
    scores.setAlignment(Pos.CENTER);
    scores.setSpacing(5);
    pane.setCenter(scores);

    var currentScoreLabel = new Label("Your Score");
    currentScoreLabel.getStyleClass().add("heading");
    var scoreText = new Text();
    scoreText.setTextAlignment(TextAlignment.CENTER);
    scoreText.textProperty().bind(score.asString());
    scoreText.getStyleClass().add("myscore");
    scores.getChildren().addAll(currentScoreLabel, scoreText);

    Label scoreLabel = new Label("High Scores");
    scoreLabel.getStyleClass().add("heading");
    scores.getChildren().add(scoreLabel);

    Scores scoreBox = new Scores();
    scores.getChildren().add(scoreBox);
    scoreBox.scoreProperty().bind(scoreProperty);
    scoreBox.usernameProperty().bind(app.usernameProperty());

    // Add play again button
    var button1 = new Button("Play Again");
    button1.setOnAction(
        (e) -> {
          app.playingProperty().set(false);
          scene.getWindow().hide();
          app.openGame();
        });

    // Add stop playing button
    var button2 = new Button("Stop Playing");
    button2.setOnAction(
        (e) -> {
          scene.getWindow().hide();
          app.playingProperty().set(false);
        });
    scores.getChildren().addAll(button1, button2);

    scene.setRoot(pane);
  }

  public Scene getScene() {
    return scene;
  }
}
