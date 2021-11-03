import java.io.BufferedReader;
import java.io.FileReader;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;

public class FlashCardReader {

	BufferedReader reader;
	ArrayList<FlashCard> cardList = new ArrayList<FlashCard>();

	public FlashCardReader(String filename) {
		try {
			reader = new BufferedReader(new FileReader(filename));
		} catch (FileNotFoundException e) {
			System.out.println(e);
		}
	}

	public String getLine() {
		try {
			return reader.readLine();
		} catch (IOException e) {
			System.out.println(e);
		}
		return "ERROR";
	}

	public boolean fileIsReady() {
		try {
			if (reader != null) {
				return reader.ready();
			}
		} catch (IOException e) {
			System.out.println(e);
		}
		return false;
	}

	public ArrayList<FlashCard> getFlashCards() {
		while (true) {
			String tmp = this.getLine();
			if (tmp == null) {
				break;
			}
			int index = tmp.indexOf(":");

			String question = tmp.substring(0, index);
			String answer = tmp.substring(index + 1, tmp.length());

			FlashCard card = new FlashCard(question, answer);
			cardList.add(card);
		}
		return cardList;
	}
}