package uk.ac.soton.comp1206.ui;

import javafx.geometry.Insets;
import javafx.scene.control.Button;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.TextField;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.VBox;

import uk.ac.soton.comp1206.scene.build.MultiplayerLobbySceneBuilder;
import static uk.ac.soton.comp1206.scene.build.MultiplayerLobbySceneBuilder.sendMessage;

/** The chat box to appear in the lobby when joining a channel. */
public class ChatBox extends VBox {

  /** Build the chat box UI on initialization. */
  public ChatBox() {
    this.setSpacing(10);
    this.setPadding(new Insets(5));
    this.getStyleClass().add("chatBox");

    // Generating UI elements
    generatePlayerList();
    generateScroller();
    generateMessagesInScroller();
    generateTextField();
    generatePaneWithButtons();
  }

  /** Generating an anchor pane with leave and start game button on the left and right side. */
  private void generatePaneWithButtons() {
    AnchorPane buttons = new AnchorPane();
    getChildren().add(buttons);

    // Generating the leave button
    Button leaveButton = new Button("Leave game");
    leaveButton.setOnAction(
        (e) -> {
          sendMessage("PART");
          sendMessage("LIST");
        });
    buttons.getChildren().add(leaveButton);
    AnchorPane.setRightAnchor(leaveButton, 0.0);

    // Generating the start button
    Button startButton = new Button("Start game");
    startButton.visibleProperty().bind(MultiplayerLobbySceneBuilder.host);
    startButton.setOnAction((e) -> MultiplayerLobbySceneBuilder.requestStart());
    buttons.getChildren().add(startButton);
    AnchorPane.setLeftAnchor(startButton, 0.0);
  }

  /** Generate text field for chat messages. */
  private void generateTextField() {
    TextField sendMessage = new TextField();
    sendMessage.setPromptText("Send a message");
    sendMessage.setOnKeyPressed(
        (e) -> {
          if (e.getCode().equals(KeyCode.ENTER)) {
            MultiplayerLobbySceneBuilder.sendMsg(sendMessage.getText());
            sendMessage.clear();
          }
        });
    getChildren().add(sendMessage);
  }

  /** Generate VBox for the chat messages inside the scroller. */
  private void generateMessagesInScroller() {
    MultiplayerLobbySceneBuilder.messages = new VBox();
    MultiplayerLobbySceneBuilder.messages.getStyleClass().add("messages");
    getChildren().add(MultiplayerLobbySceneBuilder.scroller);
    MultiplayerLobbySceneBuilder.scroller.setContent(MultiplayerLobbySceneBuilder.messages);
  }

  /** Generate the scroller that will hold the chat messages. */
  private void generateScroller() {
    MultiplayerLobbySceneBuilder.scroller = new ScrollPane();
    MultiplayerLobbySceneBuilder.scroller.getStyleClass().add("scroller");
    MultiplayerLobbySceneBuilder.scroller.setFitToWidth(true);
    MultiplayerLobbySceneBuilder.scroller.setPrefHeight(
        MultiplayerLobbySceneBuilder.gameWindow.getHeight() / 2.0);
  }

  /** Generating the player list in the channel. */
  private void generatePlayerList() {
    MultiplayerLobbySceneBuilder.playerList = new PlayerList();
    getChildren().add(MultiplayerLobbySceneBuilder.playerList);
  }
}
