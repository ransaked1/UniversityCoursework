import bugs.*;
import building.*;
import students.*;

/**
 * Class that contains the main function of the game. Takes the input arguments and orchestrates the
 * game flow.
 */
public class EcsBuildingDefence {
  public static void main(String[] args) {
    // Store all input arguments
    int topFloor = Integer.valueOf(args[0]);
    int constructionPoints = Integer.valueOf(args[1]);
    int knowledgePoints = Integer.valueOf(args[3]);
    String fileName = args[2];

    // Next wave is triggered after 8*topFloor steps after the last wave
    int nextWaveSteps = topFloor * 8;
    int steps = 1; // Steps since the last wave
    int waveCount = 0; // Curent wave count

    // Initializing the builiding, bug waves and student team
    Building building = new Building(topFloor, constructionPoints);
    ConfigReader bugWaves = new ConfigReader(fileName);
    Team team = new Team(knowledgePoints);

    // Initializing a new battle and loading/printing the first wave
    Battle battle = new Battle(building, team);
    battle.addBugs(bugWaves.getBugWaves().get(waveCount));
    waveCount++;
    battle.printGameState(0);

    // While the game is not stopped increase the step count and check that there are waves to load
    // and load them, resseting the steps to the next wave and increasing the wave count.
    while (battle.step(waveCount, bugWaves.getBugWaves().size())) {
      steps++;
      if (steps == nextWaveSteps && waveCount < bugWaves.getBugWaves().size()) {
        battle.addBugs(bugWaves.getBugWaves().get(waveCount));
        steps = 1;
        waveCount++;
      }
    }
  }
}
