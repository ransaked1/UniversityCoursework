package uk.ac.soton.comp1206.drawing;

import javafx.scene.canvas.Canvas;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.paint.Color;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;


/**
 * Create a whiteboard canvas that can be painted on as part of the Whiteboard. Extends Canvas.
 */
public class Whiteboard extends Canvas {

    private static final Logger logger = LogManager.getLogger(Whiteboard.class);

    private final GraphicsContext gc;

    /**
     * Create a new Whiteboard
     */
    public Whiteboard() {
        gc = getGraphicsContext2D();
        gc.setLineWidth(3);
        gc.setStroke(Color.BLACK);

        setOnMousePressed((e) -> {
            gc.beginPath();
            paint(e.getX(),e.getY());
        });

        setOnMouseDragged((e) -> {
            paint(e.getX(),e.getY());
            gc.stroke();
        });

        setOnMouseReleased((e) -> {
            paint(e.getX(),e.getY());
            gc.stroke();
            gc.closePath();
        });
    }

    /**
     * Create a paint point at x and y. Create a paintMessage point to communicate after painting is over.
     * @param x x point
     * @param y y point
     */
    public void paint(double x, double y) {
        gc.lineTo(x, y);
    }


    @Override
    /**
     * Override preferred width to 0
     */
    public double prefWidth(double height) {
        return 0;
    }

    @Override
    /**
     * Override preferred height to 0
     */
    public double prefHeight(double width) {
        return 0;
    }

    @Override
    /**
     * Overide the resizable to true
     */
    public boolean isResizable() {
        return true;
    }

}
