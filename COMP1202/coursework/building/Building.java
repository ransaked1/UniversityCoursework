package building;

import java.util.ArrayList;
import java.util.Collections;
import java.util.ConcurrentModificationException;
import bugs.*;

public class Building {

  int constructionPoints;
  int topFloor;
  ArrayList<Bug> bugList = new ArrayList<Bug>();

  public Building(int constructionPoints, int topFloor) {
    this.constructionPoints = constructionPoints;
    this.topFloor = topFloor;
  }

  public int getTopFloor() {
    return topFloor;
  }

  public int getConstructionPoints() {
    return constructionPoints;
  }

  public int addBug(Bug bug) {
    if (bugList.contains(bug)) {
      return -1;
    } else {
      bugList.add(bug);
      return bugList.size();
    }
  }

  public void bugsMove() {
    for (int i = 0; i < bugList.size(); i++) {
      Bug bug = bugList.get(i);
      bug.move();
      if (bug.getCurrentFloor() == this.topFloor) {
        if (bug instanceof ConcurrentModificationBug) {
          constructionPoints = constructionPoints - 2;
        } else if (bug instanceof NoneTerminationBug) {
          constructionPoints = constructionPoints - 4;
        } else {
          constructionPoints = constructionPoints - 1;
        }
        this.removeBug(bug);
      }
      if (constructionPoints <= 0) {
        constructionPoints = 0;
        break;
      }
    }
  }

  public void removeBug(Bug bug) {
    bugList.remove(bug);
  }

  public void printGameState() {
    for (Bug bug : this.getAllBugs()) {
      System.out.println(
          "Name: "
              + bug.getName()
              + "Bug current floor: "
              + bug.getCurrentFloor()
              + " Bug current step: "
              + bug.getCurrentSteps()
              + " Bug current HP: "
              + bug.getCurrentHP());
    }
    System.out.println("Building construction points: " + this.constructionPoints);
    System.out.println();
  }

  public ArrayList<Bug> getAllBugs() {
    ArrayList<Bug> tmpList = bugList;
    Collections.sort(bugList);
    Collections.sort(tmpList);
    tmpList.removeIf(bug -> bug.getCurrentFloor() == -1);
    return tmpList;
  }

  public ArrayList<Bug> getAllBugsReal() {
    Collections.sort(bugList);
    return bugList;
  }
}
