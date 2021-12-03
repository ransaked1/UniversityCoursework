import building.*;
import bugs.*;
import students.*;

import java.util.ArrayList;
import java.util.regex.Pattern;
import java.util.regex.Matcher;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class ConfigReader {

	ArrayList<ArrayList<Bug>> bugWaves = new ArrayList<ArrayList<Bug>>();

	public ArrayList<ArrayList<Bug>> getBugWaves() {
		return bugWaves;
	}

	public ConfigReader(String fileName) {
		try (BufferedReader br = new BufferedReader(new FileReader(fileName))) {
			String line;
			while ((line = br.readLine()) != null) {
				String[] bugStrings = line.split(";");
				bugWaves.add(processBugStrings(bugStrings));
			}
		} catch (IOException e) {
			System.err.println(e);
		}
	}

	private ArrayList<Bug> processBugStrings(String[] bugStrings) {
		ArrayList<Bug> bugWave = new ArrayList<Bug>();
		for (int i = 0; i < bugStrings.length; i++) {
			String[] splitAtBracket = bugStrings[i].split("\\(");
			String bugName = splitAtBracket[0];

			Matcher matcher = Pattern.compile("\\((.*?)\\)").matcher(bugStrings[i]);
			matcher.find();
			String[] insideBrackets = matcher.group(1).split(",");

			bugWave.add(processBugString(bugName, insideBrackets));
		}
		return bugWave;
	}

	private Bug processBugString(String bugName, String[] insideBrackets) {
		String bugType = insideBrackets[0];
		String bugLevel = insideBrackets[1];
		String bugSteps = insideBrackets[2];

		if (bugType.equals("CMB")) {
			return new ConcurrentModificationBug(bugName, Integer.valueOf(bugLevel), Integer.valueOf(bugSteps));
		} else if (bugType.equals("NPB")) {
			return new NullPointerBug(bugName, Integer.valueOf(bugLevel), Integer.valueOf(bugSteps));
		} else {
			return new NoneTerminationBug(bugName, Integer.valueOf(bugLevel), Integer.valueOf(bugSteps));
		}
	}
}