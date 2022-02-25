package uk.ac.soton.comp1206.utility;

import javafx.beans.property.BooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.scene.media.Media;
import javafx.scene.media.MediaPlayer;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * A utility class for quick and handy static functions
 *
 * We will be adding to this later, but you can add things that are handy here too!
 */
public class Utility {

    private static final Logger logger = LogManager.getLogger(Utility.class);
    private static SimpleBooleanProperty audioEnabled = new SimpleBooleanProperty(true);

    //The JavaFX MediaPlayer needs to be declared outside the function
    //or it gets garbage collected and stops playing sounds!
    private static MediaPlayer mediaPlayer;

    /**
     * Play an audio file
     * @param file filename to play from resources
     */
    public static void playAudio(String file) {
        if (!audioEnabled.get()) return;

        String toPlay = Utility.class.getResource("/" + file).toExternalForm();
        logger.info("Playing audio: " + toPlay);

        try {
            Media play = new Media(toPlay);
            mediaPlayer = new MediaPlayer(play);
            mediaPlayer.play();
        } catch (Exception e) {
            audioEnabled.set(false);
            e.printStackTrace();
            logger.error("Unable to play audio file, disabling audio");
        }
    }

    public static BooleanProperty audioEnabledProperty() {
        return audioEnabled;
    }

}
