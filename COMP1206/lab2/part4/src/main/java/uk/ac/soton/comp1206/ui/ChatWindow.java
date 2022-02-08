package uk.ac.soton.comp1206.ui;

import javafx.collections.ListChangeListener;
import javafx.scene.Node;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.TextField;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.*;
import javafx.scene.text.Text;
import javafx.scene.text.TextFlow;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.soton.comp1206.App;
import uk.ac.soton.comp1206.network.Communicator;
import uk.ac.soton.comp1206.utility.Utility;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

/**
 * Chat window which will display chat messages and a way to send new messages
 */
public class ChatWindow {

    private static final Logger logger = LogManager.getLogger(ChatWindow.class);
    private static final DateTimeFormatter formatter = DateTimeFormatter.ofPattern("HH:mm");

    private final App app;
    private final Scene scene;
    private final Communicator communicator;

    /**
     * Create a new Chat Window, linked to the main App and the Communicator
     * @param app the main app
     * @param communicator the communicator
     */
    public ChatWindow(App app, Communicator communicator) {
        this.app = app;
        this.communicator = communicator;

        //Setup scene with a border pane
        var pane = new BorderPane();
        this.scene = new Scene(pane,640,480);

        //Link the communicator to this window
        communicator.setWindow(this);

        TextFlow messages = new TextFlow();
        pane.setCenter(messages);
        Label message = new Label("Hello World!\n");
        messages.getChildren().add(message);

        HBox bottomPane = new HBox();
        TextField input = new TextField();
        Button sendButton = new Button("send");
        bottomPane.getChildren().add(input);
        bottomPane.getChildren().add(sendButton);
        pane.setBottom(bottomPane);

        //Set the stylesheet for this window
        String css = this.getClass().getResource("/chat.css").toExternalForm();
        scene.getStylesheets().add(css);
    }

    /**
     * Handle an incoming message from the Communicator
     * @param message The message that has been received, in the form User:Message
     */
    public void receiveMessage(String message) {
        //TODO: Handle incoming messages
    }

    /**
     * Send an outgoing message from the Chatwindow
     * @param text The text of the message to send to the Communicator
     */
    private void sendCurrentMessage(String text) {
        communicator.send(app.getUsername() + ":" + text);
    }

    /**
     * Get the scene contained inside the Chat Window
     * @return
     */
    public Scene getScene() {
        return scene;
    }
}
