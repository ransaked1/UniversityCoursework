package uk.ac.soton.comp1206.game;

import javafx.beans.property.IntegerProperty;
import javafx.beans.property.SimpleIntegerProperty;
import javafx.scene.canvas.Canvas;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.paint.Color;

public class GameBlock extends Canvas {

	private int x;
	private int y;
	private double width;
	private double height;
	private Color[] colors;

	private final IntegerProperty value = new SimpleIntegerProperty(-1);

	private boolean hover = false;

	public GameBlock(Color[] colors, int x, int y, double width, double height) {
		this.x = x;
		this.y = y;
		this.colors = colors;
		this.width = width;
		this.height = height;

		setWidth(width);
		setHeight(height);

		value.addListener((observable, oldValue, newValue) -> paint());

		paint();

		setOnMouseEntered((e) -> {
			hover = true;
			paint();
		});

		//When the mouse leaves, no longer hover
		setOnMouseExited((e) -> {
			hover = false;
			paint();
		});

		//Do initial paint
		paint();
	}

	public void paint() {
		GraphicsContext gc = getGraphicsContext2D();
		gc.clearRect(0, 0, width, height);

		if (value.get() >= 0) {
			gc.setFill(colors[value.get()]);
			gc.setStroke(Color.BLACK);
			gc.fillRect(0, 0, width, height);
			gc.strokeRect(0, 0, width, height);
		}

		if(hover) {
			gc.setFill(Color.color(1, 1, 1, 0.5));
			gc.fillRect(0, 0, width, height);
		}
	}

	public int getX() {
		return x;
	}

	public int getY() {
		return y;
	}

	public IntegerProperty valueProperty() {
		return value;
	}
}
