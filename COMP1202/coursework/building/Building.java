package building;

import java.util.ArrayList;
import java.util.Collections;
import java.util.ConcurrentModificationException;
import bugs.*;

public class Building {

  int constructionPoints;
  int topFloor;
  ArrayList<Bug> bugs = new ArrayList<Bug>();

  public Building(int topFloor, int constructionPoints) {
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
    for (Bug bug1 : bugs) {
      if (compareBugsEqual(bug1, bug)) {
        return -1;
      }
    }
    bugs.add(bug);
    return bugs.size();
  }

  public boolean compareBugsEqual(Bug bug1, Bug bug2) {
    if (bug1.getBaseSteps() == bug2.getBaseSteps()
        && bug1.getName() == bug2.getName()
        && (int) bug1.getCurrentHp() == (int) bug2.getCurrentHp()
        && bug1.getCurrentSteps() == bug2.getCurrentSteps()
        && bug1.getCurrentFloor() == bug2.getCurrentFloor()) {
      return true;
    }
    return false;
  }

  public void bugsMove() {
    ArrayList<Bug> bugsToRemove = new ArrayList<Bug>();

    for (Bug bug : bugs) {
      bug.move();
      if (bug.getCurrentFloor() == this.topFloor) {
        if (bug instanceof ConcurrentModificationBug) {
          constructionPoints = constructionPoints - 2;
        } else if (bug instanceof NoneTerminationBug) {
          constructionPoints = constructionPoints - 4;
        } else {
          constructionPoints = constructionPoints - 1;
        }
        bugsToRemove.add(bug);
      }
      if (constructionPoints <= 0) {
        removeMultipleBugs(bugsToRemove);
        constructionPoints = 0;
        break;
      }
    }
    removeMultipleBugs(bugsToRemove);
  }

  public void removeMultipleBugs(ArrayList<Bug> bugsToRemove) {
    for (Bug bug : bugsToRemove) {
      this.bugs.remove(bug);
    }
  }

  public void removeBug(Bug bug) {
    bugs.remove(bug);
  }

  public void printGameState(int attackNumber, int knowledgePoints) {
    if (attackNumber == 0) {
      System.out.println("Initial state of the game: ");
    } else {
      System.out.println("Attack number: " + attackNumber);
    }
    for (Bug bug : this.getAllBugsReal()) {
      System.out.println(
          "Name: "
              + bug.getName()
              + " Bug current floor: "
              + bug.getCurrentFloor()
              + " Bug current step: "
              + bug.getCurrentSteps()
              + " Bug current Hp: "
              + bug.getCurrentHp());
    }
    System.out.println("Team knowledge points: " + knowledgePoints);
    System.out.println("Building construction points: " + this.constructionPoints);
    System.out.println();
  }

  public ArrayList<Bug> getAllBugsReal() {
    Collections.sort(bugs);
    return bugs;
  }

  public Bug[] getAllBugs() {
    ArrayList<Bug> tmpList = new ArrayList<Bug>(bugs.size());
    Collections.sort(bugs);
    for (Bug item : bugs) {
      tmpList.add(item);
    }
    tmpList.removeIf(bug -> bug.getCurrentFloor() == -1);
    Bug[] result = new Bug[tmpList.size()];
    for (int i = 0; i < tmpList.size(); i++) {
      result[i] = tmpList.get(i);
    }
    return result;
  }
}
