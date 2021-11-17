import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintStream;
import java.util.ArrayList;

public class Quiz {

  FlashCardReader reader;
  ArrayList<FlashCard> cardList;
  Toolbox toolbox = new Toolbox();
  PrintStream ps;
  int score = 0;

  /**
   * Get filename and start a game with a cardlist.
   *
   * @param filename The filename to start a game with.
   */
  public Quiz(String filename) {
    reader = new FlashCardReader(filename);
    cardList = reader.getFlashCards();
    this.play();
  }

  /**
   * Start a game.
   */
  public void play() {
    System.out.println("Do you wish to save your answers?");
    String saveOption = toolbox.readStringFromCmd();

    if (saveOption.equals("N")) {
      for (FlashCard card : cardList) {
        System.out.println(card.getQuestion());
        String userAnswer = toolbox.readStringFromCmd();

        if (userAnswer.equals(card.getAnswer())) {
          System.out.println("Your answer is right!");
        } else {
          System.out.println("Your answer is wrong, the right answer is "
              + card.getAnswer() + ".");
        }
      }
    } else {
      this.save();
      for (FlashCard card : cardList) {
        System.out.println(card.getQuestion());
        String userAnswer = toolbox.readStringFromCmd();

        if (userAnswer.equals(card.getAnswer())) {
          ps.println(card.getQuestion() + "," + userAnswer + "," + "right");
          score += 1;
        } else {
          System.out.println("Your answer is wrong, the right answer is "
              + card.getAnswer() + ".");
          ps.println(card.getQuestion() + "," + userAnswer + "," + "wrong");
        }
      }
      float percentage = score * 100 / cardList.size();
      ps.println(
          Integer.toString(score) + "," + cardList.size() + ","
              + Float.toString(percentage));
      ps.flush();
    }
  }

  /**
   * Save the game state to file "save.txt".
   */
  public void save() {
    File file = new File("save.txt");
    if (file.exists() && file.isFile()) {
      file.delete();
    }

    try {
      file.createNewFile();
    } catch (IOException e) {
      e.printStackTrace();
    }

    try {
      this.ps = new PrintStream("save.txt");
    } catch (FileNotFoundException e) {
      e.printStackTrace();
    }
  }

  /**
   * Build a quiz object. Starts a game automatically.
   *
   * @param args Standard input string list.
   */
  public static void main(String[] args) {
    Quiz quiz = new Quiz("Questions.txt");
  }
}
