import building.*;
import bugs.*;
import students.*;

public class EcsBuildingDefence {
	public static void main(String[] args) {
		int topFloor = Integer.valueOf(args[0]);
		int constructionPoints = Integer.valueOf(args[1]);
		int knowledgePoints = Integer.valueOf(args[3]);
		String fileName = args[2];

		int nextWaveSteps = topFloor * 4;
		int steps = 1;
		int waveCount = 0;

		System.out.println(nextWaveSteps);

		Building building = new Building(topFloor, constructionPoints);
		ConfigReader bugWaves = new ConfigReader(fileName);
		Team team = new Team(knowledgePoints);

		Battle battle = new Battle(building, team);
		battle.addBugs(bugWaves.getBugWaves().get(waveCount));
		waveCount++;
		battle.printGameState(0);

		while (battle.step()) {
			steps++;
			if (steps == nextWaveSteps && waveCount < bugWaves.getBugWaves().size()) {
				battle.addBugs(bugWaves.getBugWaves().get(waveCount));
				steps = 1;
				waveCount++;
			}
		}
	}
}