package uk.ac.soton.comp1206.network;

import com.neovisionaries.ws.client.*;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.ArrayList;
import java.util.List;

import uk.ac.soton.comp1206.event.CommunicationsListener;

/**
 * Uses web sockets to talk to a web socket server and relays communication to attached listeners
 *
 * YOU DO NOT NEED TO WORRY ABOUT THIS CLASS! Leave it be :-)
 */
public class Communicator {

    private static final Logger logger = LogManager.getLogger(Communicator.class);

    /**
     * Attached communication listeners listening to messages on this Communicator. Each will be sent any messages.
     */
    private final List<CommunicationsListener> handlers = new ArrayList<>();

    private WebSocket ws = null;

    private static boolean offline;

    /**
     * Create a new communicator to the given web socket server
     *
     * @param server server to connect to
     */
    public Communicator(String server) {

        try {
            var socketFactory = new WebSocketFactory();

            //Connect to the server
            ws = socketFactory.createSocket(server);
            ws.connect();
            logger.info("Connected to " + server);

            //When a message is received, call the receiving method
            ws.addListener(new WebSocketAdapter() {
                @Override
                public void onTextMessage(WebSocket websocket, String message) {
                    Communicator.this.receive(message);
                }
                @Override
                public void onPingFrame(WebSocket webSocket, WebSocketFrame webSocketFrame) {
                    //logger.info("Ping? Pong!");
                }
            });

            //Error handling
            ws.addListener(new WebSocketAdapter() {
                @Override
                public void onTextMessage(WebSocket websocket, String message) {
                    if(message.startsWith("ERROR")) {
                        logger.error(message);
                    }
                }
                @Override
                public void handleCallbackError(WebSocket webSocket, Throwable throwable) {
                    logger.error("Callback Error:" + throwable.getMessage());
                    throwable.printStackTrace();
                }
                @Override
                public void onError(WebSocket webSocket, WebSocketException e) {
                    logger.error("Error:" + e.getMessage());
                    e.printStackTrace();
                }
            });

            offline = false;

        } catch (Exception e){
            logger.error("Socket error: " + e.getMessage());
            logger.info("Running in offline mode");

            offline = true;
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

    public static boolean isOnline() {
        return !offline;
    }

    /**
     * Add a new listener to receive messages from the server
     * @param listener the listener to add
     */
    public void addListener(CommunicationsListener listener) {
        this.handlers.add(listener);
    }

    /**
     * Clear all current listeners
     */
    public void clearListeners() {
        this.handlers.clear();
    }

    /** Receive a message from the server. Relay to any attached listeners
     *
     * @param message the message that was received
     */
    private void receive(String message) {
        logger.info("Received: " + message);
        for(CommunicationsListener handler : handlers) {
            handler.receiveCommunication(message);
        }
    }

}
