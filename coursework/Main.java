import building.*;
import bugs.*;
import students.*;

public class Main {
  public static void main(String[] args) {
    Building building = new Building(7, 2);
    building.addBug(new ConcurrentModificationBug("ConcBug", 3, 2));
    building.addBug(new NoneTerminationBug("TermBug", 2, 4));
    building.addBug(new NullPointerBug("PointerBug", 2, 2));
    building.addBug(new ConcurrentModificationBug("ConcBug2", 3, 1));
    building.addBug(new NullPointerBug("PointerBug2", 2, 2));
    building.printGameState();

    AiStudent student = new AiStudent(2);

    while (building.getConstructionPoints() != 0 && building.getAllBugs().size() != 0) {
      student.defence(building);
      building.bugsMove();
      building.printGameState();
    }
  }
}
