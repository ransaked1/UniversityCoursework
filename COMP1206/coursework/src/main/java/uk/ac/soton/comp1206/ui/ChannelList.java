package uk.ac.soton.comp1206.ui;

import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.geometry.Insets;
import javafx.scene.control.TextField;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.VBox;
import javafx.scene.text.Text;

import java.util.*;

import static uk.ac.soton.comp1206.scene.build.MultiplayerLobbySceneBuilder.sendMessage;
import uk.ac.soton.comp1206.scene.build.MultiplayerLobbySceneBuilder;
import uk.ac.soton.comp1206.util.Multimedia;

/** Channel list UI object. */
public class ChannelList extends VBox {

  /** Channel property and list as a hash map */
  private final HashMap<String, Text> channelList = new HashMap<>();
  private final StringProperty channel = new SimpleStringProperty();

  /** Initialize the UI elements of the channel list. */
  public ChannelList() {
    // Set up the general properties
    this.setSpacing(12);
    this.setPadding(new Insets(5));
    this.setPrefWidth(320);

    // Generate the text field for the name of the channel
    TextField newChannelField = generateNewChannelField();

    // Binding it
    bindChannelProperty();
    bindNewChannelField(newChannelField);
  }

  /**
   * Adding the channel to the list of channels.
   *
   * @param channelName the name of the new channel
   */
  public void add(String channelName) {
    // Check the channel name is unique
    if (!channelList.containsKey(channelName)) {
      Text channelText = new Text(channelName);
      channelText.getStyleClass().add("channelItem");
      getChildren().add(channelText);

      // Join the newly created channel
      channelList.put(channelName, channelText);
      channelText.setOnMouseClicked((e) -> MultiplayerLobbySceneBuilder.requestJoin(channelName));
    }
  }

  /**
   * Update the channel list with new channel names and remove the old ones.
   *
   * @param channels the new channel list
   */
  public void update(List<String> channels) {
    Set<String> existing = channelList.keySet();

    // Check that there are changes to the channel list
    if (existing.size() != channels.size() || !existing.containsAll(channels)) {
      Set<String> remove = new HashSet<>();
      Iterator<String> iter = existing.iterator();

      String channelName;
      while (iter.hasNext()) {
        channelName = iter.next();
        // Log channels to be removed
        if (!channels.contains(channelName)) {
          remove.add(channelName);
        }
      }

      // Remove the channels from the list
      iter = remove.iterator();
      while (iter.hasNext()) {
        channelName = iter.next();
        getChildren().remove(channelList.get(channelName));
      }
      channelList.keySet().removeAll(remove);

      // Add the new channels to the list
      iter = channels.iterator();
      while (iter.hasNext()) {
        channelName = iter.next();
        add(channelName);
      }
    }
  }

  /**
   * Add a new channel to the list and send the message to the server.
   *
   * @param newChannelField the new channel in the list
   */
  private void bindNewChannelField(TextField newChannelField) {
    newChannelField.setOnKeyPressed(
        (e) -> {
          if (e.getCode().equals(KeyCode.ENTER)) {

            //Send request to the server
            sendMessage("CREATE " + newChannelField.getText().trim());
            sendMessage("LIST");

            // Play success sound
            Multimedia.playAudio("place_success.wav");

            // Hide the new channel field
            newChannelField.setVisible(false);
            newChannelField.clear();
          }
        });
  }

  /** Bind to the channel property to style the created channel. */
  private void bindChannelProperty() {
    channel.addListener(
        (observable, oldValue, channelName) -> {
          // Remove styling on all channel names
          for (Text all : channelList.values()) {
            all.getStyleClass().remove("selected");
          }

          // Add style to the selected channel
          if (!channelName.isEmpty()) {
            Text channelText = channelList.get(channelName);
            channelText.getStyleClass().add("selected");
          }
        });
  }

  /**
   * Generate the text field to create new channels.
   *
   * @return the text field UI element
   */
  private TextField generateNewChannelField() {
    Text newChannel = new Text("Create Lobby");
    newChannel.getStyleClass().add("channelItem");
    getChildren().add(newChannel);

    TextField newChannelField = new TextField();
    newChannelField.setVisible(false);
    getChildren().add(newChannelField);

    // Play sound and select the newly created channel
    newChannel.setOnMouseClicked(
        (e) -> {
          newChannelField.setVisible(true);
          newChannelField.requestFocus();
          Multimedia.playAudio("button_press.wav");
        });
    return newChannelField;
  }

  /**
   * Getter for the crated channel property.
   *
   * @return the property for the new channel
   */
  public StringProperty getChannelProperty() {
    return channel;
  }
}
