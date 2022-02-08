package uk.ac.soton.comp1206;

import javafx.animation.Animation;
import javafx.animation.KeyFrame;
import javafx.animation.Timeline;
import javafx.application.Application;
import javafx.application.Platform;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.layout.StackPane;
import javafx.scene.layout.TilePane;
import javafx.stage.Stage;
import javafx.util.Duration;

import java.text.DateFormat;
import java.text.SimpleDateFormat;


/**
 * JavaFX App
 */
public class App extends Application {

    @Override
    public void start(Stage stage) {
        var javaVersion = SystemInfo.javaVersion();
        var javafxVersion = SystemInfo.javafxVersion();

        stage.setTitle("creating buttons");

        // create a button
        Button b = new Button("button");

        // create a label
        Label l = new Label("button not selected");

        // action event
        EventHandler<ActionEvent> event = new EventHandler<ActionEvent>() {
            public void handle(ActionEvent e)
            {
                Platform.exit();
            }
        };

        b.setOnAction(event);

        // create a stack pane
        TilePane r = new TilePane();

        // add button
        r.getChildren().add(b);
        r.getChildren().add(l);

        var scene = new Scene(new StackPane(r), 640, 480);
        stage.setScene(scene);
        stage.show();
    }

    public static void main(String[] args) {
        launch();
    }

}