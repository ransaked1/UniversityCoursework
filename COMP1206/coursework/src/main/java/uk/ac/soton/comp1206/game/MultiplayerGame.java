package uk.ac.soton.comp1206.game;

import javafx.util.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.soton.comp1206.component.GameBlock;
import uk.ac.soton.comp1206.network.Communicator;
import uk.ac.soton.comp1206.util.Constants;

import java.util.ArrayDeque;

import static javafx.application.Platform.*;

/** A multiplayer game extending the base Game object adding online functionality. */
public class MultiplayerGame extends Game {

  private static final Logger logger = LogManager.getLogger(MultiplayerGame.class);

  /** The network communicator of the multiplayer game. */
  private final Communicator communicator;

  /** The incoming game pieces from the server */
  private final ArrayDeque<GamePiece> incoming = new ArrayDeque<>();

  /** Status of the game */
  private boolean started;

  /**
   * Constructor for the multiplayer game.
   *
   * @param communicator the communicator
   * @param cols the number of columns
   * @param rows the number of rows
   */
  public MultiplayerGame(Communicator communicator, int cols, int rows) {
    super(cols, rows);
    this.communicator = communicator;
    communicator.addListener((message) -> runLater(() -> receiveMessage(message.trim())));
  }

  /** Overriding the game initialisation of the Challenge Scene. */
  @Override
  public void initialiseGame() {
    logger.info("Initialising game");
    // Initializing bindable values
    score.set(0);
    level.set(0);
    lives.set(3);
    mult.set(1);

    initialPieces();
  }

  /**
   * Handling the block clicked.
   *
   * @param gameBlock the block that was clicked
   * @return the status of the block clicked
   */
  public boolean blockClicked(GameBlock gameBlock) {
    boolean result = super.blockClicked(gameBlock);
    communicator.send("BOARD " + encode());
    return result;
  }

  /**
   * Send message to server to generate a new game piece.
   *
   * @return the new piece
   */
  public GamePiece spawnPiece() {
    communicator.send("PIECE");
    return incoming.pop();
  }

  /**
   * Message receiver and parser from the server.
   *
   * @param message Server message
   */
  private void receiveMessage(String message) {
    logger.info("Received message: {}", message);
    String[] components = message.split(" ", 2);
    String command = components[0];
    String data;
    if (command.equals("PIECE") && components.length > 1) {
      data = components[1];
      receivePiece(Integer.parseInt(data));
    } else if (command.equals("SCORES") && components.length > 1) {
      data = components[1];
      receiveScores(data);
    }
  }

  /**
   * Parsing received scores.
   *
   * @param data Raw text data
   */
  private void receiveScores(String data) {
    scores.clear();
    String[] scoreLines = data.split("\\R");

    // Parsing score lines from server
    for (String scoreLine : scoreLines) {
      String[] components = scoreLine.split(":");
      String player = components[0];
      int score = Integer.parseInt(components[1]);

      logger.info("Received score: {} = {}", player, score);
      scores.add(new Pair<>(player, score));
    }

    scores.sort((a, b) -> b.getValue().compareTo(a.getValue()));
  }

  /**
   * Parsing received game piece.
   *
   * @param block the number of the block
   */
  private void receivePiece(int block) {
    GamePiece piece = GamePiece.createPiece(block);
    logger.info("Received next piece: {}", piece);
    incoming.add(piece);
    logger.info("Next piece: {}", incoming);

    if (!started && incoming.size() > 2) {
      logger.info("Initial pieces received, the game begins");
      followingPiece = spawnPiece();
      nextPiece();
      started = true;
    }
  }

  /**
   * Encode the status of the board to be sent to the server.
   *
   * @return the encoded message
   */
  private String encode() {
    StringBuilder board = new StringBuilder();

    for (int x = 0; x < cols; ++x) {
      for (int y = 0; y < rows; ++y) {
        int tmp = grid.get(x, y);
        board.append(tmp).append(" ");
      }
    }
    return board.toString().trim();
  }

  /**
   * Requesting new game pieces from the server.
   *
   * <p>Change the size of the request to adjust the load on the server.
   */
  private void initialPieces() {
    for (int i = 0; i < Constants.PIECE_REQUEST_SIZE; ++i) {
      communicator.send("PIECE");
    }
  }
}
