package students;

import bugs.*;
import building.*;

import java.lang.Math;
import java.util.ArrayList;

/**
 * SeStudent class that implements the Student interface and extends the AbstractStudent class with
 * functions for calculating the damage on level up and defence of the building.
 */
public class SeStudent extends AbstractStudent implements Student {

  /**
   * Constructor that sets the student base attack, delay and level.
   *
   * @param level The level of the student to create.
   */
  public SeStudent(int level) {
    super(5, 6, level);
  }

  /**
   * Calculate potential damage on level-up.
   *
   * @return The damage.
   */
  public int levelUpDamage() {
    // Check if the next defence is a special attack
    if (delayCounter + 1 != delay) {
      return this.getDamage();
    } else {
      return 0; // Zero damage, only pushes back
    }
  }

  /**
   * Defending the building by attacking the top bug or using a special attack.
   *
   * @param building The building object to defend.
   * @return The knowledge points gained after defending.
   */
  public int defence(Building building) {
    int totalKnowledgePts = 0;

    // Exit if there is no bug to attack
    if (building.getAllBugs().length == 0) {
      return 0;
    }

    // Check the type of attack and apply it
    Bug[] bugList = building.getAllBugs();
    Bug bug = bugList[0];
    if (delayCounter < delay) {
      delayCounter = delayCounter + 1;
      return damageBug(building, totalKnowledgePts, bug, 1); // Standard attack
    } else {
      delayCounter = 1;
      return bugsStepBackSuper(bugList); // Push bugs back 2 steps
    }
  }

  /**
   * Helper function that pushes the bugs in the building 2 steps backwards.
   *
   * @param bugList The list of bugs in the building.
   * @return 0 knowledge points earned.
   */
  private int bugsStepBackSuper(Bug[] bugList) {
    int stopIndex = 4; // Stop at the fifth bug by default

    // Adjust if the list of bugs has less than 5 bugs to push
    if (stopIndex >= bugList.length) {
      stopIndex = bugList.length - 1;
    }

    // Slow down each bug in the list 2 steps until reaching the stop index
    for (int i = 0; i <= stopIndex; i++) {
      Bug bug = bugList[i];
      bug.slowDown(2);
    }
    return 0;
  }
}
