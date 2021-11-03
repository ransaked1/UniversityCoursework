import java.util.ArrayList;

public class Quiz {

	FlashCardReader reader;
	ArrayList<FlashCard> cardList;
	Toolbox toolbox = new Toolbox();

	public Quiz(String filename) {
		reader = new FlashCardReader(filename);
		cardList = reader.getFlashCards();
		this.play();
	}

	public void play() {
		for (FlashCard card : cardList) {
			System.out.println(card.getQuestion());
			String userAnswer = toolbox.readStringFromCmd();

			if (userAnswer.equals(card.getAnswer())) {
				System.out.println("Your answer is right!");
			} else {
				System.out.println("Your answer is wrong, the right answer is " + card.getAnswer());
			}
		}
	}

	public static void main(String[] args) {
		Quiz quiz = new Quiz("Questions.txt");
	}
}