import java.util.ArrayList;

public class Quiz {

  FlashCardReader reader;
  ArrayList<FlashCard> cardList;
  Toolbox toolbox = new Toolbox();

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
    for (FlashCard card : cardList) {
      System.out.println(card.getQuestion());
      String userAnswer = toolbox.readStringFromCmd();

      if (userAnswer.equals(card.getAnswer())) {
        System.out.println("Your answer is right!");
      } else {
        System.out.println("Your answer is wrong, the right answer is "
            + card.getAnswer());
      }
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
