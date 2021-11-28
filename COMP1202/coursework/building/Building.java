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
    for (Bug bugInList : bugList) {
      if (checkBugsEqual(bug, bugInList)) {
        return -1;
      }
    }
    bugList.add(bug);
    return bugList.size();
  }

  public boolean checkBugsEqual(Bug bug1, Bug bug2) {
    if (bug1.getBaseSteps() == bug2.getBaseSteps() && bug1.getLevel() == bug2.getLevel()
        && bug1.getName() == bug2.getName() && bug1.getCurrentHP() == bug2.getCurrentHP()) {
      return true;
    }
    return false;
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
    for (Bug bug : this.getAllBugsReal()) {
      System.out.println(
          "Name: "
              + bug.getName()
              + " Bug current floor: "
              + bug.getCurrentFloor()
              + " Bug current step: "
              + bug.getCurrentSteps()
              + " Bug current HP: "
              + bug.getCurrentHP());
    }
    System.out.println("Building construction points: " + this.constructionPoints);
    System.out.println();
  }

  public ArrayList<Bug> getAllBugsReal() {
    Collections.sort(bugList);
    return bugList;
  }

  public ArrayList<Bug> getAllBugs() {
    ArrayList<Bug> tmpList = new ArrayList<Bug>();
    Collections.sort(bugList);
    for (Bug bug : bugList) {
      if (bug.getCurrentFloor() != -1) {
        tmpList.add(bug);
      }
    }
    Collections.sort(tmpList);
    return tmpList;
  }
}
