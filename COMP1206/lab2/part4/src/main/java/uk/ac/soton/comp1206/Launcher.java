package uk.ac.soton.comp1206;

/**
 * Launcher class (which is used to launch the main App)
 *
 * This is used to allow creation of a shaded jar file,
 * which cannot extend from the JavaFX Application
 *
 * You do not need to worry about this class
 */
public class Launcher {

    /**
     * Start the main application and pass the arguments along
     * @param args commandline arguments
     */
    public static void main(String[] args) {
        App.main(args);
    }

}
