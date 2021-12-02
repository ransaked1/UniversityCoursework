import building.*;
import bugs.*;
import students.*;

import java.util.ArrayList;

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

    Team team = new Team(1000);
    try {
      team.recruitNewStudent();
      team.recruitNewStudent();
      team.recruitNewStudent();
      team.recruitNewStudent();
      ArrayList<Student> studentList = team.getStudents();
      team.upgrade(studentList.get(0));
      team.upgrade(studentList.get(0));
      team.upgrade(studentList.get(0));
    } catch (Exception e) {
      System.out.println(e);
    }
    building.printGameState(0, team.getKnowledgePoints());


    int attackNumber = 1;
    int points = team.getKnowledgePoints();
    while (building.getConstructionPoints() != 0 && building.getAllBugsReal().size() != 1 && attackNumber <= 10) {
      //building.bugsMove();
      points = team.studentsAct(building);
      building.printGameState(attackNumber, points);
      attackNumber++;
      // System.out.println(building.getAllBugsReal().size() + " " +
      // building.getConstructionPoints());
    }
  }
}
