package students;

import bugs.*;
import building.*;

import java.util.ArrayList;
import java.lang.Math;

/**
 * AiStudent class that implements the Student interface and extends the AbstractStudent class with
 * functions for calculating the damage on level up and defence of the building.
 */
public class AiStudent extends AbstractStudent implements Student {

  /**
   * Constructor that sets the student base attack, delay and level.
   *
   * @param level The level of the student to create.
   */
  public AiStudent(int level) {
    super(7, 7, level);
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
      return this.getDamage() * 3; // The damage to the top 3 bugs
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
      delayCounter += 1;
      return damageBug(building, totalKnowledgePts, bug, 1); // Simple attack
    } else {
      return specialPower(building, totalKnowledgePts, bugList); // Special attack
    }
  }

  /**
   * Helper method that applies a special attack to a bug and removes it if it's killed. The special
   * attack applies standard damage to the top 3 bugs.
   *
   * @param building The builidng to remove the bug from.
   * @param totalKnowledgePts The knowledge points gained so far.
   * @param bugList The list of bugs in the building.
   * @return The total knowledge points after the attack.
   */
  public int specialPower(Building building, int totalKnowledgePts, Bug[] bugList) {
    delayCounter = 1;
    int stopIndex = 2; // Stop at the third bug by default

    // Adjust if the list of bugs has less than 3 bugs to attack
    if (stopIndex >= bugList.length) {
      stopIndex = bugList.length - 1;
    }

    // Apply standard damage to the top-most bugs until reaching the stop index
    for (int i = 0; i <= stopIndex; i++) {
      Bug bug = bugList[i];
      totalKnowledgePts += damageBug(building, totalKnowledgePts, bug, 1);
    }
    return totalKnowledgePts;
  }
}
