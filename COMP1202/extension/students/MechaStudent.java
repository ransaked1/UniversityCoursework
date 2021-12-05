package students;

import bugs.*;
import building.*;

import java.util.ArrayList;
import java.lang.Math;

/**
 * MechaStudent class that implements the Student interface and extends the AbstractStudent class
 * with functions for calculating the damage on level up and defence of the building. Its special
 * power attacks the top bug with a 2x multiplier, if it kills it attacks the next bug and so on.
 */
public class MechaStudent extends AbstractStudent implements Student {

  /**
   * Constructor that sets the student base attack, delay and level.
   *
   * @param level The level of the student to create.
   */
  public MechaStudent(int level) {
    super(10, 9, level);
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
   * attack pplies 2x damage to the top bug. If it kills, go to the next bug and to the same.
   *
   * @param building The builidng to remove the bug from.
   * @param totalKnowledgePts The knowledge points gained so far.
   * @param bugList The list of bugs in the building.
   * @return The total knowledge points after the attack.
   */
  public int specialPower(Building building, int totalKnowledgePts, Bug[] bugList) {
    delayCounter = 1;

    // Apply standard damage to the top bug and decide if to go to the next one.
    for (int i = 0; i <= bugList.length - 1; i++) {
      Bug bug = bugList[i];
      if (bug.getCurrentHp() <= getDamage() * 2) {
        totalKnowledgePts += damageBug(building, totalKnowledgePts, bug, 2);
      } else {
        totalKnowledgePts += damageBug(building, totalKnowledgePts, bug, 2);
        return totalKnowledgePts; // Stop if there is no kill
      }
    }
    return totalKnowledgePts;
  }
}
