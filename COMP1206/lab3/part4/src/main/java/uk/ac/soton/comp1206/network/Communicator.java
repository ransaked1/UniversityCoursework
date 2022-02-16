package uk.ac.soton.comp1206.network;

import com.neovisionaries.ws.client.WebSocket;
import com.neovisionaries.ws.client.WebSocketAdapter;
import com.neovisionaries.ws.client.WebSocketFactory;
import javafx.application.Platform;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import javafx.scene.control.Alert;
import uk.ac.soton.comp1206.ui.ChatWindow;

/**
 * Uses web sockets to talk to a web socket server and relays communication to the ChatWindow
 *
 * YOU DO NOT NEED TO WORRY ABOUT THIS CLASS! Leave it be :-)
 */
public class Communicator {

    private static final Logger logger = LogManager.getLogger(Communicator.class);

    private WebSocket ws = null;
    private String server;
    private ChatWindow window;

    /**
     * Create a new communicator to the given web socket server
     *
     * @param server server to connect to
     */
    public Communicator(String server) {
        this.server = server;

        try {
            var socketFactory = new WebSocketFactory();

            //Connect to the server
            ws = socketFactory.createSocket(server);
            ws.connect();
            logger.info("Connected to " + server);

            //When a message is received, call the receive method
            ws.addListener(new WebSocketAdapter() {
                @Override
                public void onTextMessage(WebSocket websocket, String message) throws Exception {
                    Communicator.this.receive(websocket, message);
                }
            });

        } catch (Exception e){
            logger.error("Socket error: " + e.getMessage());
            e.printStackTrace();

            Alert error = new Alert(Alert.AlertType.ERROR,"Unable to communicate with the ECSChat server\n\n" + e.getMessage() + "\n\nPlease ensure you are connected to the VPN");
            error.showAndWait();
            System.exit(1);
        }
    }

    /** Send a message to the server
     *
     * @param message Message to send
     */
    public void send(String message) {
        logger.info("Sending message: " + message);

        ws.sendText(message);
    }

    /** Receive a message from the server. Relay to the Chat Window to handle
     *
     * @param websocket the socket
     * @param message the message that was received
     */
    private void receive(WebSocket websocket, String message) {
        logger.info("Received: " + message);

        Platform.runLater(() -> {
            window.receiveMessage(message);
        });
    }

    /**
     * Set the ChatWindow so the communicator can message it appropriately
     * @param window chat window
     */
    public void setWindow(ChatWindow window) {
        this.window = window;
    }
}
