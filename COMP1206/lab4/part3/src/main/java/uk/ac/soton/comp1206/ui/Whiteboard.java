package uk.ac.soton.comp1206.ui;

import javafx.scene.canvas.Canvas;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.paint.Color;
import javafx.scene.paint.Paint;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.soton.comp1206.App;
import uk.ac.soton.comp1206.network.Communicator;

import java.awt.*;

public class Whiteboard extends Canvas {

	private static final Logger logger = LogManager.getLogger(Whiteboard.class);

	private final GraphicsContext gc;

	public Whiteboard() {
		gc = getGraphicsContext2D();
		gc.setLineWidth(3);

		setOnMousePressed((e) -> {
			gc.setStroke(Color.BLACK);
			gc.beginPath();
			gc.lineTo(e.getX(), e.getY());
		});

		setOnMouseDragged((e) -> {
			gc.lineTo(e.getX(), e.getY());
			gc.stroke();
		});

		setOnMouseReleased((e) -> {
			gc.lineTo(e.getX(), e.getY());
			gc.stroke();
			gc.closePath();
		});
	}
}
