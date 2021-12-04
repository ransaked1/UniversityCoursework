package building;

import bugs.*;

import java.util.ArrayList;
import java.util.Collections;
import java.util.ConcurrentModificationException;

/**
 * Building class managing the bugs inside it with an ArrayList and providing the building floors
 * and construciton points it has.
 */
public class Building {

  // Constants for bug damage to the building
  static final int CMB_DAMAGE = 2;
  static final int NTB_DAMAGE = 4;
  static final int NPB_DAMAGE = 1;

  ArrayList<Bug> bugs = new ArrayList<Bug>(); // The list of bugs loaded
  int constructionPoints;
  int topFloor;

  /**
   * Building constructor taking the top floor and construction points (building HP) it has.
   *
   * @param topFloor
   * @param constructionPoints
   */
  public Building(int topFloor, int constructionPoints) {
    this.constructionPoints = constructionPoints;
    this.topFloor = topFloor;
  }

  public int getConstructionPoints() {
    return constructionPoints;
  }

  public int getTopFloor() {
    return topFloor;
  }

  /**
   * Adding a new bug to the bug list. If an exact same bug is already in the list don't add it.
   *
   * @param bug The bug to add.
   * @return The new size of the bug list after the addition.
   */
  public int addBug(Bug bug) {
    // Return -1 if the bug is already in the list
    for (Bug bug1 : bugs) {
      if (compareBugsEqual(bug1, bug)) {
        return -1;
      }
    }
    bugs.add(bug); // Add the new bug to the list of bugs
    return bugs.size();
  }

  private boolean compareBugsEqual(Bug bug1, Bug bug2) {
    if (bug1.getBaseSteps() == bug2.getBaseSteps()
        && bug1.getName() == bug2.getName()
        && (int) bug1.getCurrentHp() == (int) bug2.getCurrentHp()
        && bug1.getCurrentSteps() == bug2.getCurrentSteps()
        && bug1.getCurrentFloor() == bug2.getCurrentFloor()) {
      return true;
    }
    return false;
  }

  /**
   * Remove a bug from the list of bugs in the game.
   *
   * @param bug The bug to remove from the game.
   */
  public void removeBug(Bug bug) {
    bugs.remove(bug);
  }

  /**
   * Move all the bugs in the game one step and check if there has to be applied any damage to the
   * building.
   */
  public void bugsMove() {
    ArrayList<Bug> bugsToRemove = new ArrayList<Bug>(); // All bugs that reached the top floor

    // Go through each bug and move it one step and apply its damage if necessary
    for (Bug bug : bugs) {
      bug.move();
      applyBuildingDamage(bugsToRemove, bug); // Apply the bugs damage if needed

      // If the building is destroyed set construction points to 0 and exit the loop
      if (constructionPoints <= 0) {
        constructionPoints = 0;
        break;
      }
    }
    removeMultipleBugs(bugsToRemove); // Remove all the bugs that reached the top floor
  }

  /**
   * Helper method to apply all the bug's type damage to the building if the top floor is reached.
   *
   * @param bugsToRemove The list of bugs to later be removed from the game.
   * @param bug The bug to be checked.
   */
  private void applyBuildingDamage(ArrayList<Bug> bugsToRemove, Bug bug) {
    // Check that the bug reached the top floor
    if (bug.getCurrentFloor() == this.topFloor) {
      // Check the type of bug and apply the respective damage to the building
      if (bug instanceof ConcurrentModificationBug) {
        constructionPoints -= CMB_DAMAGE;
      } else if (bug instanceof NoneTerminationBug) {
        constructionPoints -= NTB_DAMAGE;
      } else {
        constructionPoints -= NPB_DAMAGE;
      }
      bugsToRemove.add(bug); // Add the bug to the remove list
    }
  }

  /**
   * Helper method to remove multiple bugs from the game.
   *
   * @param bugsToRemove A list of bugs to be removed.
   */
  private void removeMultipleBugs(ArrayList<Bug> bugsToRemove) {
    for (Bug bug : bugsToRemove) {
      this.bugs.remove(bug);
    }
  }

  /**
   * Finds all the bugs alive in the building sorted by position. Doesn't consider the bugs outside
   * the bulding (floor -1). Also returns a simple array to comply to the specification.
   *
   * @return An Array of all the bugs alive in the building sorted by their position.
   */
  public Bug[] getAllBugs() {
    ArrayList<Bug> tmpList = new ArrayList<Bug>(bugs.size()); // A temporary ArrayList

    Collections.sort(bugs); // Sorts the bugs by position

    // Makes a copy of the current bug list
    for (Bug item : bugs) {
      tmpList.add(item);
    }

    // Remove the bugs that are on floor -1
    tmpList.removeIf(bug -> bug.getCurrentFloor() == -1);

    // Transform the ArrayList into a simple Array of bugs
    Bug[] result = new Bug[tmpList.size()];
    for (int i = 0; i < tmpList.size(); i++) {
      result[i] = tmpList.get(i);
    }
    return result;
  }

  /**
   * A variation of getAllBugs that returns all the bugs in the game not only the ones inside the
   * building. Also sorts the bug list.
   *
   * @return An ArrayList of all the bugs in the game sorted by their position.
   */
  public ArrayList<Bug> getAllBugsReal() {
    Collections.sort(bugs);
    return bugs;
  }
}
