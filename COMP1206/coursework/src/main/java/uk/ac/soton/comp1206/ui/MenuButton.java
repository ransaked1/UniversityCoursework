package uk.ac.soton.comp1206.ui;

import javafx.scene.Group;
import javafx.scene.text.Text;
import uk.ac.soton.comp1206.util.Multimedia;

public class MenuButton extends Group {
  private final Text text;
  private Runnable action;
  private final boolean disabled;

  /**
   * Construct a new menu button.
   *
   * @param name the text on the button
   * @param disabled set if the button should be disabled
   */
  public MenuButton(String name, boolean disabled) {
    text = new Text(name);
    text.getStyleClass().add("menuItem");
    getChildren().add(text);

    this.disabled = disabled;
    if (disabled) {
      disableButton();
      text.getStyleClass().add("disabled");
    }
  }

  /** Deselect a menu item. Apply appropriate style. */
  public void deselect() {
    text.getStyleClass().remove("selected");
  }

  /**
   * Trigger sound when a menu button is pressed.
   *
   * @param action button action
   */
  public void setOnAction(Runnable action) {
    this.action = action;
    if (!disabled) {
      setOnMouseClicked(
          (e) -> {
            Multimedia.playAudio("button_press.wav");
            this.action.run();
          });
    }
  }

  /** Dummy method for the disabled buttons in the menu scene. */
  @SuppressWarnings("EmptyMethod")
  private void disableButton() {}
}
