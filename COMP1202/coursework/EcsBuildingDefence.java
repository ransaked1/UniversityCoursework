import building.*;
import bugs.*;
import students.*;

public class EcsBuildingDefence {
	public static void main(String[] args) {

		Building building = new Building(Integer.valueOf(args[0]), Integer.valueOf(args[1]));
		Team team = new Team(Integer.valueOf(args[3]));

		Battle battle = new Battle(building, team);
		battle.addBugs();
		battle.printGameState(0);

		while (battle.step()) {
		}
	}
}