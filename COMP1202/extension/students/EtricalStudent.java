package students;

import bugs.*;
import building.*;

import java.util.ArrayList;
import java.lang.Math;

/**
 * EtronicStudent class that implements the Student interface and extends the AbstractStudent class
 * with functions for calculating the damage on level up and defence of the building. Its special
 * power repairs the construction 2 + level construction points.
 */
public class EtricalStudent extends AbstractStudent implements Student {

  /**
   * Constructor that sets the student base attack, delay and level.
   *
   * @param level The level of the student to create.
   */
  public EtricalStudent(int level) {
    super(6, 10, level);
  }

  /**
   * Calculate potential damage on level-up.
   *
   * @return The damage.
   */
  public int levelUpDamage() {
    return this.getDamage(); // No special changes
  }

  /**
   * Defending the building by attacking the top bug or repairing the building.
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
      return specialPower(building); // Repair building
    }
  }

  /**
   * Helper method that repairs the building by 2 * level construction points.
   *
   * @param building The builidng to repair.
   * @return 0 knowledge points earned.
   */
  public int specialPower(Building building) {
    delayCounter = 1;
    building.repair(2 * level);
    return 0;
  }
}
