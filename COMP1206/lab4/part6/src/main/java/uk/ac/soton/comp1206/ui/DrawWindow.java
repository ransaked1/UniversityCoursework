package uk.ac.soton.comp1206.ui;

import javafx.application.Platform;
import javafx.event.ActionEvent;
import javafx.scene.Scene;
import javafx.scene.image.WritableImage;
import javafx.scene.layout.*;
import javafx.stage.FileChooser;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.soton.comp1206.App;
import javafx.embed.swing.SwingFXUtils;
import uk.ac.soton.comp1206.drawing.PaintMessage;
import uk.ac.soton.comp1206.drawing.Whiteboard;
import uk.ac.soton.comp1206.network.Communicator;


import javax.imageio.ImageIO;
import java.awt.image.RenderedImage;
import java.io.*;

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

        //Register a drawing listener to receive DRAW messages
        communicator.addListener((message) -> Platform.runLater(() -> {
            if(message.startsWith("DRAW")) {
                this.receiveDraw(message);
            }
        }));

        var pane = new BorderPane();


        //Create a canvas and expand to width and height
        canvas = new Whiteboard();
        canvas.widthProperty().bind(pane.widthProperty());
        canvas.heightProperty().bind(pane.heightProperty());
        pane.setCenter(canvas);

        //Send message when drawing complete
        canvas.setOnDrawComplete(this::sendPaintMessage);

        this.scene = new Scene(pane,800,480);
    }

    /**
     * Send a paint message from a complet paint operation through the communicator
     * @param message paint message
     */
    private void sendPaintMessage(PaintMessage message) {
        String encoded = message.encode();
        logger.info("Sending message: " + encoded);
        communicator.send("DRAW " + encoded);
    }

    /**
     * Save the drawing to a file from the canvas
     * @param event
     */
    private void saveDrawing(ActionEvent event) {
        FileChooser fileChooser = new FileChooser();

        //Set extension filter
        FileChooser.ExtensionFilter extFilter =
                new FileChooser.ExtensionFilter("png files (*.png)", "*.png");
        fileChooser.getExtensionFilters().add(extFilter);

        //Show save file dialog
        File file = fileChooser.showSaveDialog(getScene().getWindow());

        if (file != null) {
            try {
                WritableImage writableImage = new WritableImage((int) canvas.getWidth(), (int) canvas.getHeight());
                canvas.snapshot(null, writableImage);
                RenderedImage renderedImage = SwingFXUtils.fromFXImage(writableImage, null);
                ImageIO.write(renderedImage, "png", file);
            } catch (IOException ex) {
                ex.printStackTrace();
            }
        }
    }

    /**
     * Receive incoming draw message and handle
     * @param message full message received
     */
    private void receiveDraw(String message) {
        String drawing = message.replace("DRAW","").trim();
        logger.info("Received drawing: " + drawing);

        PaintMessage paintMessage = new PaintMessage(drawing);
        paintMessage.paint(canvas.getGraphicsContext2D());
    }

    /**
     * Get the drawing window scene
     * @return drawing window scene
     */
    public Scene getScene() {
        return scene;
    }
}
