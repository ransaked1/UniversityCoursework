package uk.ac.soton.comp1206.ui;

import java.util.ArrayList;

import javafx.geometry.Pos;
import javafx.scene.Group;
import javafx.scene.layout.VBox;
import uk.ac.soton.comp1206.network.Communicator;

/**
 * UI element holding the menu buttons.
 */
public class MenuWindow extends Group {
  private final VBox box = new VBox(10);
  private final ArrayList<MenuButton> items = new ArrayList<>();

  /**
   * Set up a centered menu element that hold the buttons that can be selected.
   */
  public MenuWindow() {
    box.setAlignment(Pos.CENTER);
    box.getStyleClass().add("menu");

    this.setOnMouseMoved((e) -> {
      for (MenuButton item : items) {
        item.deselect();
      }
    });
    getChildren().add(box);
  }

  /**
   * Add menu items to the window element.
   *
   * @param label the button label
   * @param action button action
   * @param isOnline the button status
   */
  public void add(String label, Runnable action, boolean isOnline) {
    MenuButton item;

    //Check if the menu item should be disabled
    if (isOnline && !Communicator.isOnline()) {
      item = new MenuButton(label, true);
    } else {
      item = new MenuButton(label, false);
    }

    // Add the button to the window element
    items.add(item);
    item.setOnAction(action);
    box.getChildren().add(item);
  }
}
