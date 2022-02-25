package uk.ac.soton.comp1206.drawing;

import javafx.geometry.Point2D;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.paint.Color;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.LinkedHashSet;

public class PaintMessage {

    private static final Logger logger = LogManager.getLogger(PaintMessage.class);

    private Color colour = Color.BLACK;
    private final LinkedHashSet<Point2D> points = new LinkedHashSet<>();

    /** Create a new Paint Message from a received message
     *
     * @param message The incoming message received (after the DRAW header)
     */
    public PaintMessage(String message) {
        try {
            String[] coordinates = message.split(" ");

            //Get the colour
            colour = Color.web(coordinates[0]);
            logger.info("Colour set to: " + colour.toString());

            //Get the coordinates
            for (String coordinate : coordinates) {
                String[] parts = coordinate.split(",");
                if(parts.length != 2) continue;
                double x = Double.parseDouble(parts[0]);
                double y = Double.parseDouble(parts[1]);
                addPoint(x, y);
            }
            logger.info("Received {} points",coordinates.length);
        } catch (Exception e) {
            e.printStackTrace();
            logger.error("Unable to decode:" + e.getMessage());
        }
    }

    /**
     * Create a new empty Paint Message (ready for filling)
     */
    public PaintMessage() {

    }

    /**
     * Add a drawing point, as a pair of doubles, to this Paint Message
     * @param x x-coordinate
     * @param y y-coordinate
     */
    public void addPoint(double x, double y) {
        points.add(new Point2D(x, y));
    }

    /**
     * Encode the set of drawing points into an encoded string
     * @return encoded string
     */
    public String encode() {
        logger.info("Encoding {} points",points.size());
        StringBuilder result = new StringBuilder(colourToHex(colour) + " ");
        for(Point2D point : points) {
            result.append(point.getX()).append(",").append(point.getY()).append(" ");
        }
        return result.toString().trim();
    }

    /**
     * Use the provided Graphics Context from a canvas to draw all the points held in the message
     * @param gc Graphics Context from a canvas
     */
    public void paint(GraphicsContext gc) {
        logger.info("Painting {} points",points.size());
        gc.setStroke(colour);
        gc.beginPath();
        for(var point : points) {
            gc.lineTo(point.getX(),point.getY());
            gc.stroke();
        }
        gc.closePath();
    }

    /**
     * Convert a colour object into a hex representation ready for encoding
     * @param colour colour to encode
     * @return hex colour code
     */
    private String colourToHex(Color colour) {
        int r = ((int) Math.round(colour.getRed()     * 255)) << 24;
        int g = ((int) Math.round(colour.getGreen()   * 255)) << 16;
        int b = ((int) Math.round(colour.getBlue()    * 255)) << 8;
        int a = ((int) Math.round(colour.getOpacity() * 255));
        return String.format("#%08X", (r + g + b + a));
    }

    /**
     * Set the colour represented by this Paint Message
     * @param selected chosen colour
     */
    public void setColour(Color selected) {
        this.colour = selected;
    }

}
