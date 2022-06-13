package uk.ac.soton.comp1206.scene;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;
import javafx.scene.control.Alert;
import javafx.scene.control.Alert.AlertType;
import javafx.scene.input.KeyCode;
import javafx.scene.text.Text;
import javafx.scene.text.TextFlow;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.soton.comp1206.scene.build.MultiplayerLobbySceneBuilder;
import uk.ac.soton.comp1206.ui.GameWindow;
import uk.ac.soton.comp1206.util.Constants;
import uk.ac.soton.comp1206.util.Multimedia;

import static javafx.application.Platform.*;

/**
 * The Multiplayer Lobby scene. Holds the UI for the lobby and send server request to update the UI.
 */
public class MultiplayerLobbyScene extends MultiplayerLobbySceneBuilder {

  private static final Logger logger = LogManager.getLogger(MultiplayerLobbyScene.class);

  /**
   * Utility variables.
   */
  private static final DateTimeFormatter formatter = DateTimeFormatter.ofPattern("HH:mm");
  private Timer timer;

  /**
   * Create a new Multiplayer Lobby scene
   *
   * @param gameWindow the Game Window
   */
  public MultiplayerLobbyScene(GameWindow gameWindow) {
    super(gameWindow);
    communicator = gameWindow.getCommunicator();
    logger.info("Creating Lobby Scene");
  }

  /**
   * Stop sending requests to the server.
   */
  private void cleanup() {
    if (timer != null) {
      timer.purge();
      timer.cancel();
      timer = null;
    }
  }

  /**
   * Initialize the Lobby Scene.
   */
  public void initialise() {
    // Leave scene on ESC key
    this.scene.setOnKeyPressed((e) -> {
      if (e.getCode().equals(KeyCode.ESCAPE)) {
        this.leave();
        this.cleanup();
        gameWindow.startMenu();
      }
    });

    // Request the list of channels
    sendMessage("LIST");
    communicator.addListener((message) -> runLater(() -> receiveMessage(message.trim())));

    // Timer for the next channel list request
    setupRefreshTimer();
  }

  /**
   * Create a timer with the Refresh Interval set.
   */
  private void setupRefreshTimer() {
    TimerTask refreshChannels = new TimerTask() {
      public void run() {
        //logger.info("Refreshing channel list");
        sendMessage("LIST");
      }
    };
    timer = new Timer();
    timer.schedule(refreshChannels, 0, Constants.REFRESH_INTERVAL);
  }

  /**
   * Leave the channel.
   */
  private void leave() {
    if (!channel.isEmpty().get()) {
      sendMessage("PART");
    }
  }

  /**
   * Parse server message and execute commands.
   *
   * @param message Raw server message
   */
  private void receiveMessage(String message) {
    logger.info("Received message: {}", message);
    String[] components = message.split(" ", 2);
    String command = components[0];
    String error;

    // Get the list of channels or clear it
    if (command.equals("CHANNELS")) {
      if (components.length <= 1) {
        channelList.update(new ArrayList<>());
      } else {
        error = components[1];
        receiveChannelList(error);
      }
    }

    // Start the multiplayer game
    if (command.equals("START")) {
      startGame();
    }

    // Change the nickname
    if (command.equals("NICK") && components.length > 1) {
      error = components[1];
      if (!error.contains(":")) {
        this.setName(components[1]);
      }
    }

    // Change the channel host
    if (command.equals("HOST")) {
      host.set(true);
    }

    // Join a channel
    if (command.equals("JOIN")) {
      host.set(false);
      error = components[1];
      //logger.info("Joined {}", error);
      join(error);
    }

    // Leave a channel
    if (command.equals("PARTED")) {
      channel.set("");
    }

    // Get the list of users in the channel
    if (command.equals("USERS") && components.length > 1) {
      setUsers(components[1]);
    }

    // Receive a user message from the server
    if (command.equals("MSG")) {
      error = components[1];
      receiveMsg(error);
    }

    // Receive an error from the server and display it as an alert
    if (command.equals("ERROR")) {
      error = components[1];
      logger.error(error);

      // Create and show alert
      var alert = new Alert(AlertType.ERROR, error);
      alert.showAndWait();
    }

  }

  /** Add user to the player list when joining the channel. */
  private void setUsers(String data) {
    logger.info("Channel user list: {}", data);
    String[] players = data.split("\\R");
    List<String> list = Arrays.asList(players);
    playerList.set(list);
    Multimedia.playAudio("message.wav");
  }

  /**
   * Set name for the player.
   *
   * @param name the new name of the user
   */
  private void setName(String name) {
    MultiplayerLobbySceneBuilder.name.set(name);
  }

  /**
   * Receive a message from the server.
   *
   * Parses the message and formats it appropriately to be displayed in the scroller.
   *
   * @param data the raw message received
   */
  private void receiveMsg(String data) {
    String[] components = data.split(":", 2);
    TextFlow message = new TextFlow();
    message.getStyleClass().add("message");

    // Parse message text
    Text timestamp = new Text("[" + formatter.format(LocalDateTime.now()) + "] ");
    Text nick = new Text("<" + components[0] + "> ");
    Text msg = new Text(components[1]);
    message.getChildren().addAll(timestamp, nick, msg);
    messages.getChildren().add(message);

    // Play sound on message receive
    Multimedia.playAudio("message.wav");

    // Add text to the scroller
    scroller.getParent().layout();
    scroller.layout();
    scroller.setVvalue(1.0D);
  }

  /**
   * Receive the list of channels.
   *
   * @param data Raw channel list data as a string
   */
  private void receiveChannelList(String data) {
    //logger.info("Received channel list: {}", data);
    String[] channels = data.split("\\R");
    List<String> list = Arrays.asList(channels);
    channelList.update(list);
  }

  /**
   * Add user to the channel and display the welcome message in the chat.
   *
   * @param channelName the name of the user joining
   */
  private void join(String channelName) {
    channelList.add(channelName);
    channel.set(channelName);
    messages.getChildren().clear();

    Text intro = new Text("""
      Welcome to the lobby
      Type /nick [your nickname] to change your nickname

      """);
    messages.getChildren().add(intro);
  }

  /**
   * Stop channel updates and start the game.
   */
  private void startGame() {
    logger.info("Game starting!");
    cleanup();
    gameWindow.startMultiplayerGame();
  }
}
