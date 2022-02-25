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

public class UserList extends VBox {

	private static VBox users;
	private javafx.scene.control.ScrollPane scroller;
	private static javafx.scene.control.TextField username;
	private CheckBox mute;

	public UserList() {
		setPrefWidth(160);
		setSpacing(20);
		setPadding(new Insets(10,10,10,10));
		getStyleClass().add("userList");
    setAlignment(Pos.TOP_CENTER);

		build();
	}

	public void build() {
		//Add ECS logo
		var image = new ImageView(new Image(this.getClass().getResource("/ECS.png").toExternalForm()));
		image.setPreserveRatio(true);
		image.setFitWidth(64);
		getChildren().add(image);

		//Add username field
		username = new javafx.scene.control.TextField();
		getChildren().add(username);

		//Add list of users
		users = new VBox();
		users.setSpacing(20);
		users.setPadding(new Insets(10, 10, 10, 10));

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

	public static TextField getUsernameField() {
		return username;
	}

	public static void addUser(String username) {
		var userBox = new HBox();
		var userImage = new ImageView(new Image(UserList.class.getResource("/User.png").toExternalForm()));
		userImage.setPreserveRatio(true);
		userImage.setFitHeight(16);
		userBox.getChildren().add(userImage);
		userBox.getStyleClass().add("user");
		userBox.getSpacing();
    HBox.setHgrow(userImage, Priority.NEVER);

		var user = new Text(username);
		userBox.getChildren().add(user);
		HBox.setHgrow(userImage, Priority.ALWAYS);

		users.getChildren().add(userBox);
	}
}
