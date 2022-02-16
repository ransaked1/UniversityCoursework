package uk.ac.soton.comp1206.ui;

import javafx.collections.ListChangeListener;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
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
    private final TextFlow messages;
    private final TextField messageToSend;

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

        //Create a horizontal bar with a text box and send button
        messageToSend = new TextField();
        messageToSend.setPromptText("Enter message");
        Button sendMessage = new Button("Send");
        HBox sendMessageBar = new HBox();
        sendMessageBar.getChildren().add(messageToSend);
        sendMessageBar.getChildren().add(sendMessage);
        HBox.setHgrow(messageToSend,Priority.ALWAYS);
        pane.setBottom(sendMessageBar);

        //Create a textflow to hold all messages
        messages = new TextFlow();
        pane.setCenter(messages);

        //Make the send button send a message
        sendMessage.setOnAction((event)-> ChatWindow.this.sendCurrentMessage(messageToSend.getText()));

        //Make pressing enter on the text field send a message
        messageToSend.setOnKeyPressed((event) -> {
            if (event.getCode() != KeyCode.ENTER) return;
            sendCurrentMessage(messageToSend.getText());
        });

        //Set the stylesheet for this window
        String css = this.getClass().getResource("/chat.css").toExternalForm();
        scene.getStylesheets().add(css);
    }

    /**
     * Handle an incoming message from the Communicator
     * @param message The message that has been received, in the form User:Message
     */
    public void receiveMessage(String message) {
        //Play incoming audio sound
        Utility.playAudio("incoming.mp3");

        //Create a new Text GUI element and set its text to the received message. We add a new line to the end
        Text receivedMessage = new Text(message + "\n");

        //Add this message to the TextFlow
        messages.getChildren().add(receivedMessage);
    }

    /**
     * Send an outgoing message from the Chatwindow
     * @param text The text of the message to send to the Communicator
     */
    private void sendCurrentMessage(String text) {
        //Send the message to the communicator
        communicator.send(app.getUsername() + ":" + text);
        
        //Clear the text input box
        messageToSend.clear();
    }

    /**
     * Get the scene contained inside the Chat Window
     * @return
     */
    public Scene getScene() {
        return scene;
    }
}
