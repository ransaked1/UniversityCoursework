package uk.ac.soton.comp1206.ui;

import javafx.scene.Scene;
import javafx.scene.layout.BorderPane;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.soton.comp1206.App;
import uk.ac.soton.comp1206.network.Communicator;

public class DrawWindow {

	private static final Logger logger = LogManager.getLogger(DrawWindow.class);

	private final Scene scene;
	private final Communicator communicator;
	private final Whiteboard canvas;

	public DrawWindow(App app,Communicator communicator) {
		this.communicator = communicator;
		var pane = new BorderPane();

		canvas = new Whiteboard();
		canvas.widthProperty().bind(pane.widthProperty());
		canvas.heightProperty().bind(pane.heightProperty());
		pane.setCenter(canvas);

		this.scene = new Scene(pane, 800, 480);
	}

	public Scene getScene() {
		return scene;
	}
}
