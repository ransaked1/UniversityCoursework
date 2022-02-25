package uk.ac.soton.comp1206.ui;

import javafx.application.Platform;
import javafx.scene.Scene;
import javafx.scene.layout.*;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.soton.comp1206.App;
import uk.ac.soton.comp1206.drawing.PaintMessage;
import uk.ac.soton.comp1206.drawing.Whiteboard;
import uk.ac.soton.comp1206.network.Communicator;

/**
 * Whiteboard drawing window
 */
public class DrawWindow {

    private static final Logger logger = LogManager.getLogger(DrawWindow.class);

    private final Scene scene;
    private final Communicator communicator;
    private final Whiteboard canvas;

    /**
     * Create a new drawing window
     * @param app app
     * @param communicator communicator
     */
    public DrawWindow(App app, Communicator communicator) {
        this.communicator = communicator;

        communicator.addListener((message) -> Platform.runLater(() -> {
            if (message.startsWith("DRAW")) {
                this.recieveDraw(message);
            }
        }));

        var pane = new BorderPane();

        //Create a canvas and expand to width and height
        canvas = new Whiteboard();
        canvas.widthProperty().bind(pane.widthProperty());
        canvas.heightProperty().bind(pane.heightProperty());
        pane.setCenter(canvas);

        canvas.setOnDrawComplete(this::sendPaintMessage);

        this.scene = new Scene(pane,800,480);
    }

    private void recieveDraw(String message) {
        String drawing = message.replace("DRAW", "").trim();
        logger.info("Received drawing: " + drawing);

        PaintMessage paintMessage = new PaintMessage(drawing);
        paintMessage.paint(canvas.getGraphicsContext2D());
    }

    private void sendPaintMessage(PaintMessage message) {
        String encoded = message.encode();
        logger.info("Sending message: " + encoded);
        communicator.send("DRAW" + encoded);
    }

    /**
     * Get the drawing window scene
     * @return drawing window scene
     */
    public Scene getScene() {
        return scene;
    }
}
