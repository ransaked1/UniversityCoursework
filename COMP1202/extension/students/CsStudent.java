package students;

import bugs.*;
import building.*;

import java.util.ArrayList;
import java.lang.Math;

/**
 * CsStudent class that implements the Student interface and extends the AbstractStudent class with
 * functions for calculating the damage on level up and defence of the building.
 */
public class CsStudent extends AbstractStudent implements Student {

  /**
   * Constructor that sets the student base attack, delay and level.
   *
   * @param level The level of the student to create.
   */
  public CsStudent(int level) {
    super(6, 6, level);
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
      return this.getDamage() * 4; // Special attack with 4x the damage
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
      return damageBug(building, totalKnowledgePts, bug, 1); // Standard attack
    } else {
      return specialPower(building, totalKnowledgePts, bug, 4); // 4x the damage
    }
  }

  public int specialPower(Building building, int totalKnowledgePts, Bug bug, int multiplier) {
    delayCounter = 1;
    totalKnowledgePts += damageBug(building, totalKnowledgePts, bug, multiplier);
    return totalKnowledgePts;
  }
}
