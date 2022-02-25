package uk.ac.soton.comp1206.game;

import javafx.beans.property.*;
import javafx.collections.ListChangeListener;
import javafx.geometry.Pos;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import javafx.scene.text.Text;
import javafx.scene.text.TextAlignment;
import javafx.util.Pair;

/**
 * The Score List links in with a Scores ArrayList and displays a list of names and points
 */
public class Scores extends VBox {

	public final SimpleListProperty<Pair<String,String>> scores = new SimpleListProperty<>();
	private final StringProperty username = new SimpleStringProperty();

	/**
	 * Create a new Score List
	 */
	public Scores() {
		getStyleClass().add("scorelist");
		setAlignment(Pos.CENTER);
		setSpacing(2);

		scores.addListener((ListChangeListener<? super Pair<String, String>>) (c) -> update());
	}


	public void update() {
		getChildren().clear();

		int top10 = 0;
		for(Pair<String,String> score : scores) {

			top10++;
			if(top10 > 10) break;

			HBox scoreBox = new HBox();
			//scoreBox.getStyleClass().add("scoreitem");
			scoreBox.setAlignment(Pos.CENTER);
			scoreBox.setSpacing(10);

			var name = new Text(score.getKey());
			if(username.get() != null && username.get().equals(name.getText())) {
				//name.getStyleClass().add("myscorer");
			}
			//name.getStyleClass().add("scorer");
			name.setTextAlignment(TextAlignment.CENTER);
			HBox.setHgrow(name, Priority.ALWAYS);

			var points = new Text(score.getValue());
			//points.getStyleClass().add("points");
			points.setTextAlignment(TextAlignment.CENTER);
			HBox.setHgrow(points,Priority.ALWAYS);

			scoreBox.getChildren().addAll(name,points);

			getChildren().add(scoreBox);
		}
	}

	public ListProperty<Pair<String,String>> scoreProperty() {
		return scores;
	}

	public Property<String> usernameProperty() {
		return username;
	}
}