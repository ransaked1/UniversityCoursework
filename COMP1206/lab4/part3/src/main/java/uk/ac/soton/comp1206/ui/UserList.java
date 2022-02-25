package uk.ac.soton.comp1206.ui;

import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.control.CheckBox;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.TextField;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import javafx.scene.text.Text;
import uk.ac.soton.comp1206.utility.Utility;

/**
 * Create a new User List as a Virtual Box
 */
public class UserList extends VBox {

    private final VBox users;
    private final ScrollPane scroller;
    private final TextField username;
    private final CheckBox mute;

    private final int width = 200;

    /**
     * Create a new user list
     */
    public UserList() {

        //Set default properties
        setPrefWidth(width);
        setSpacing(20);
        setPadding(new Insets(10,10,10,10));
        getStyleClass().add("userlist");
        setAlignment(Pos.TOP_CENTER);

        //Add the ECS Logo
        var image = new ImageView(new Image(this.getClass().getResource("/ECS.png").toExternalForm()));
        image.setPreserveRatio(true);
        image.setFitWidth(64);
        getChildren().add(image);

        //Add the username field to change username
        username = new TextField();
        getChildren().add(username);

        //Add a list of users
        users = new VBox();
        users.setSpacing(20);
        users.setPadding(new Insets(10,10,10,10));

        //Add a scroll pane for the list of users
        scroller = new ScrollPane();
        scroller.setContent(users);
        scroller.setFitToWidth(true);
        scroller.getStyleClass().add("userlist-pane");
        getChildren().add(scroller);

        //Add a mute checkbox
        mute = new CheckBox("Notifications");
        mute.selectedProperty().bindBidirectional(Utility.audioEnabledProperty());
        getChildren().add(mute);
    }

    /**
     * Get the username field from the user list
     * @return username field
     */
    public TextField getUsernameField() {
        return username;
    }

    /**
     * Add a new user to the user list
     * @param username user to add
     */
    public void addUser(String username) {
        var userBox = new HBox();
        var userImage = new ImageView(new Image(getClass().getResource("/User.png").toExternalForm()));
        userImage.setPreserveRatio(true);
        userImage.setFitHeight(16);
        userBox.getChildren().add(userImage);
        userBox.getStyleClass().add("user");
        userBox.setSpacing(10);
        HBox.setHgrow(userImage, Priority.NEVER);


        var user = new Text(username);
        userBox.getChildren().add(user);
        HBox.setHgrow(user,Priority.ALWAYS);

        users.getChildren().add(userBox);
    }


}
