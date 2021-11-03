import java.io.BufferedReader;
import java.io.FileReader;
import java.io.FileNotFoundException;
import java.io.IOException;

public class FlashCardReader {

	BufferedReader reader;

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
}