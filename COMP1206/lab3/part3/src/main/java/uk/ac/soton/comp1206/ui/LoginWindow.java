package uk.ac.soton.comp1206.ui;

import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.fxml.FXMLLoader;
import javafx.scene.control.TextField;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.soton.comp1206.App;

import java.net.URL;
import java.util.ResourceBundle;

/**
 * Displays the Login Window, to collect the username and then start the chat
 */
public class LoginWindow implements Initializable {

    private static final Logger logger = LogManager.getLogger(LoginWindow.class);
    private final App app;

    Scene scene = null;
    Parent root = null;

    /**
     * Create a new Login Window, linked to the main app. This should get the username of the user.
     * @param app the main app
     */
    public LoginWindow(App app) {
        this.app = app;

        //Load the Login Window GUI
        try {
            //Instead of building this GUI programmatically, we are going to use FXML
            var loader = new FXMLLoader(getClass().getResource("/login.fxml"));

            //TODO: Fill in login.fxml

            //Link the GUI in the FXML to this class
            loader.setController(this);
            root = loader.load();
        } catch (Exception e) {
            //Handle any exceptions with loading the FXML
            logger.error("Unable to read file: " + e.getMessage());
            e.printStackTrace();
            System.exit(1);
        }

        //We are the login window
        scene = new Scene(root);
    }

    /**
     * Get the scene contained inside the Login Window
     * @return login window scene
     */
    public Scene getScene() {
        return scene;
    }

    /**
     * Handle what happens when the user presses the login button
     * @param event button clicked
     */
    @FXML protected void handleLogin(ActionEvent event) {
        //TODO: Link this to the login button
        String user = "Guest";
        if(user.isBlank()) return;
        app.setUsername(user);
        app.openChat();
    }

    /**
     * Handle what happens when the user presses enter on the username field
     * @param event key pressed
     */
    @FXML protected void handleEnter(KeyEvent event) {
        //TODO: Link this to pressing enter on the username text field
        if(event.getCode() != KeyCode.ENTER) return;
        handleLogin(null);
    }

    /**
     * Initialise the Login Window
     * @param url
     * @param bundle
     */
    @Override
    public void initialize(URL url, ResourceBundle bundle) {
        //TODO: Any setting up of the window when it is initialised
    }
}
