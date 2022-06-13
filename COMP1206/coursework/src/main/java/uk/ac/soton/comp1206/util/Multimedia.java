package uk.ac.soton.comp1206.util;

import javafx.scene.image.Image;
import javafx.scene.media.Media;
import javafx.scene.media.MediaPlayer;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.Objects;

/**
 * Multimedia class that manages sound and image resources.
 */
public class Multimedia {

  private static final Logger logger = LogManager.getLogger(Multimedia.class);

  /**
   * Audio variables.
   */
  private static boolean audioEnabled = true;
  private static MediaPlayer mediaPlayer;
  private static MediaPlayer backgroundPlayer;

  /**
   * Basic constructor.
   */
  public Multimedia() {}

  /**
   * Stop all music.
   */
  public static void stopAll() {
    if (mediaPlayer != null) {
      mediaPlayer.stop();
    }

    if (backgroundPlayer != null) {
      backgroundPlayer.stop();
    }
  }

  /**
   * Play music file on loop.
   *
   * @param music the music file name
   */
  public static void startBackgroundMusic(String music, boolean loop) {
    if (audioEnabled) {
      //logger.info("Starting music: " + music);
      if (backgroundPlayer != null) {
        backgroundPlayer.stop();
      }

      try {
        var musicFile =
            Objects.requireNonNull(Multimedia.class.getResource("/music/" + music))
                .toExternalForm();
        var play = new Media(musicFile);
        backgroundPlayer = new MediaPlayer(play);
        backgroundPlayer.setVolume(Constants.BACKGROUND_VOLUME);
        if (loop) {
          backgroundPlayer.setCycleCount(-1);
        }

        backgroundPlayer.play();
      } catch (Exception e) {
        audioEnabled = false;
        e.printStackTrace();
        logger.error("Unable to play audio. Disabling.");
      }
    }
  }

  /**
   * Play audio sound given its name
   *
   * @param sound the name of the sound
   */
  public static void playAudio(String sound) {
    if (audioEnabled) {
      var musicFile =
          Objects.requireNonNull(Multimedia.class.getResource("/sounds/" + sound)).toExternalForm();
      logger.info("Playing sound: " + musicFile);

      try {
        Media play = new Media(musicFile);
        mediaPlayer = new MediaPlayer(play);
        mediaPlayer.play();
      } catch (Exception e) {
        audioEnabled = false;
        e.printStackTrace();
        logger.error("Unable to play audio. Disabling.");
      }
    }
  }

  /**
   * Load image given its name.
   *
   * @param image the name of the image file
   * @return the image object
   */
  public static Image getImage(String image) {
    try {
      return new Image(
          Objects.requireNonNull(Multimedia.class.getResource("/images/" + image))
              .toExternalForm());
    } catch (Exception e) {
      e.printStackTrace();
      logger.error("Unable to load image: {}", image);
      return null;
    }
  }
}
