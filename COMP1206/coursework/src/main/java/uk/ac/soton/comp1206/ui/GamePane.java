package uk.ac.soton.comp1206.ui;

import javafx.geometry.Pos;
import javafx.scene.layout.*;
import javafx.scene.transform.Scale;
import javafx.scene.transform.Translate;

/**
 * The Game Pane is a special pane which will scale anything inside it to the screen and maintain the aspect ratio.
 *
 * Drawing will be scaled appropriately.
 *
 * This takes the worry about the layout out and will allow the game to scale to any resolution easily.
 *
 * It uses the width and height given which should match the main window size. This will be the base drawing resolution,
 * but will be scaled up or down as the window is resized.
 *
 * You should not need to modify this class
 */
public class GamePane extends StackPane {

    private final int width;
    private final int height;
    private double scalar = 1;

    /**
     * Create a new scalable GamePane with the given drawing width and height.
     * @param width width
     * @param height height
     */
    public GamePane(int width, int height) {
        super();
        this.width = width;
        this.height = height;

        getStyleClass().add("gamepane");
        setAlignment(Pos.TOP_LEFT);
    }

    /**
     * Update the scalar being used by this draw pane
     * @param scalar scalar
     */
    protected void setScalar(double scalar) {
        this.scalar = scalar;
    }

    /**
     * Use a Graphics Transformation to scale everything inside this pane. Padding is added to the edges to maintain
     * the correct aspect ratio and keep the display centred.
     */
    @Override
    public void layoutChildren() {
        super.layoutChildren();

        //Work out the scale factor height and width
        var scaleFactorHeight = getHeight() / height;
        var scaleFactorWidth = getWidth() / width;

        //Work out whether to scale by width or height
        setScalar(Math.min(scaleFactorHeight, scaleFactorWidth));

        //Set up the scale
        Scale scale = new Scale(scalar,scalar);

        //Get the parent width and height
        var parentWidth = getWidth();
        var parentHeight = getHeight();

        //Get the padding needed on the top and left
        var paddingLeft = (parentWidth - (width * scalar)) / 2.0;
        var paddingTop = (parentHeight - (height * scalar)) / 2.0;

        //Perform the transformation
        Translate translate = new Translate(paddingLeft, paddingTop);
        scale.setPivotX(0);
        scale.setPivotY(0);
        getTransforms().setAll(translate, scale);
    }

}
