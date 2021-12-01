import building.*;
import bugs.*;
import students.*;

public class Main {
  public static void main(String[] args) {
    Building building = new Building(4, 5);
    building.addBug(new ConcurrentModificationBug("ConcBug1", 2, 0));
    building.addBug(new NoneTerminationBug("TermBug1", 1, 0));
    building.addBug(new NullPointerBug("PointerBug1", 3, 0));
    building.addBug(new ConcurrentModificationBug("ConcBug2", 1, 3));
    building.addBug(new NoneTerminationBug("TermBug2", 1, 2));
    building.addBug(new NullPointerBug("PointerBug2", 1, 4));
    building.bugsMove();
    building.bugsMove();
    building.bugsMove();
    building.bugsMove();
    //    building.addBug(new NullPointerBug("PointerBug3", 2, 3));
    //    building.addBug(new NullPointerBug("PointerBug3", 2, 3));
    building.printGameState(0, 0);

    CyberStudent student = new CyberStudent(3);

    int attackNumber = 1;
    int points;
    while (building.getConstructionPoints() != 0 && building.getAllBugsReal().size() != 1) {
      //building.bugsMove();
      points = student.defence(building);
      building.printGameState(attackNumber, points);
      attackNumber++;
      // System.out.println(building.getAllBugsReal().size() + " " +
      // building.getConstructionPoints());
    }
  }
}
