package uk.ac.soton.comp1206.scene.build;

import javafx.beans.property.BooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.control.ScrollPane;
import javafx.scene.layout.*;
import javafx.scene.text.Text;
import javafx.scene.text.TextAlignment;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.soton.comp1206.network.Communicator;
import uk.ac.soton.comp1206.scene.base.BaseScene;
import uk.ac.soton.comp1206.ui.ChannelList;
import uk.ac.soton.comp1206.ui.ChatBox;
import uk.ac.soton.comp1206.ui.GameWindow;
import uk.ac.soton.comp1206.ui.PlayerList;

/**
 * Builder for the Lobby Scene.
 *
 * <p>Constructs and binds UI elements in the lobby scene.
 */
public abstract class MultiplayerLobbySceneBuilder extends BaseScene {

  private static final Logger logger = LogManager.getLogger(MultiplayerLobbySceneBuilder.class);

  /** Simple UI elements */
  public static ScrollPane scroller = null;
  public static VBox messages = null;

  /** Custom UI objects. */
  public static ChannelList channelList;
  public static PlayerList playerList;

  /** Bindable properties. */
  public static final StringProperty channel = new SimpleStringProperty("");
  public static final BooleanProperty host = new SimpleBooleanProperty(false);
  public static final StringProperty name = new SimpleStringProperty();

  /** Communicator for online commands */
  protected static Communicator communicator;

  /**
   * Create a new scene, passing in the GameWindow the scene will be displayed in.
   *
   * @param gameWindow the game window
   */
  public MultiplayerLobbySceneBuilder(GameWindow gameWindow) {
    super(gameWindow);
  }

  /** Build the Lobby window. */
  @Override
  public void build() {
    logger.info("Building " + this.getClass().getName());

    // Setting up the pane
    var pane = setupPane();

    // Layout generation
    GridPane gridPane = generateLayout();

    // UI elements generation
    generateTitle(pane);
    generateListTitle(pane, gridPane);
    generateChannelList(gridPane);
    generateChannelTitle(gridPane);
    generateLobbyChat(gridPane);
  }

  /**
   * Generate the lobby chat UI
   *
   * @param gridPane the grid pane to add to
   */
  private void generateLobbyChat(GridPane gridPane) {
    ChatBox chatBox = new ChatBox();
    gridPane.add(chatBox, 1, 1);
    chatBox.visibleProperty().bind(channel.isNotEmpty());
    GridPane.setHgrow(chatBox, Priority.ALWAYS);
  }

  /**
   * Generate the channel title.
   *
   * @param gridPane add the title on top of the chat box
   */
  private void generateChannelTitle(GridPane gridPane) {
    Text lobbyText = new Text();
    lobbyText.textProperty().bind(channel);
    lobbyText.setTextAlignment(TextAlignment.CENTER);
    lobbyText.getStyleClass().add("heading");
    gridPane.add(lobbyText, 1, 0);
  }

  /**
   * Generate the channel list UI.
   *
   * @param gridPane list below the list title
   */
  private void generateChannelList(GridPane gridPane) {
    channelList = new ChannelList();
    channelList.getChannelProperty().bind(channel);
    gridPane.add(channelList, 0, 1);
  }

  /**
   * Generating list title.
   *
   * @param pane the border pane to add to
   * @param gridPane the grid pane to add to the main border pane
   */
  private void generateListTitle(BorderPane pane, GridPane gridPane) {
    Text channelText = new Text("Available games");
    channelText.setTextAlignment(TextAlignment.CENTER);
    channelText.getStyleClass().add("title");
    channelText.getStyleClass().add("small");
    gridPane.add(channelText, 0, 0);
    pane.setCenter(gridPane);
  }

  /**
   * Generating grid pane layout.
   *
   * @return the grid pane object
   */
  private GridPane generateLayout() {
    GridPane gridPane = new GridPane();
    gridPane.setHgap(8);
    gridPane.setVgap(8);
    gridPane.setPadding(new Insets(7, 7, 7, 7));
    return gridPane;
  }

  /**
   * Generate the window title.
   *
   * @param pane the pane to add to the top of
   */
  private void generateTitle(BorderPane pane) {
    Text multiplayerText = new Text("Multiplayer");
    BorderPane.setAlignment(multiplayerText, Pos.CENTER);
    multiplayerText.setTextAlignment(TextAlignment.CENTER);
    multiplayerText.getStyleClass().add("title");
    pane.setTop(multiplayerText);
  }

  /**
   * Sending message to the server.
   *
   * @param message the message to send
   */
  public static void sendMessage(String message) {
    logger.info("Sending message: {}", message);
    communicator.send(message);
  }

  /**
   * Parsing the chat message and encoding it to a server message.
   *
   * @param message the chat message
   */
  public static void sendMsg(String message) {
    if (message.startsWith("/")) {
      String[] components = message.split(" ", 2);
      String command = components[0].toLowerCase();

      if (command.equals("/nick") && components.length > 1) {
        sendMessage("NICK " + components[1]);
      } else if (command.equals("/start") && host.get()) {
        sendMessage("START");
      } else if (command.equals("/part")) {
        sendMessage("PART");
      }
    } else {
      sendMessage("MSG " + message);
    }
  }

  /**
   * Request to join a channel.
   *
   * @param channelName the name of the channel
   */
  public static void requestJoin(String channelName) {
    if (!channel.get().equals(channelName)) {
      host.set(false);
      sendMessage("JOIN " + channelName);
    }
  }

  /** Request server to start the game. */
  public static void requestStart() {
    logger.info("Requesting game start");
    sendMessage("START");
  }
}
