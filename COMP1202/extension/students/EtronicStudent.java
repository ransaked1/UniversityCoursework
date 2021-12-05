package students;

import bugs.*;
import building.*;

import java.util.ArrayList;
import java.lang.Math;

/**
 * EtronicStudent class that implements the Student interface and extends the AbstractStudent class
 * with functions for calculating the damage on level up and defence of the building. Its special
 * power gives the building immunity to damage for a round. The power is triggered every 16 - level
 * rounds.
 */
public class EtronicStudent extends AbstractStudent implements Student {

  /**
   * Constructor that sets the student base attack, delay and level.
   *
   * @param level The level of the student to create.
   */
  public EtronicStudent(int level) {
    super(6, 15, level);
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
   * Defending the building by attacking the top bug or making the building immune.
   *
   * @param building The building object to defend.
   * @return The knowledge points gained after defending.
   */
  public int defence(Building building) {
    delay = 16 - level; // Update the delay every round
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
      return specialPower(building); // Special power
    }
  }

  /**
   * Makes the building immune to bug damage every (16 - level) rounds
   *
   * @param building The builidng to apply immunity to.
   * @return The total knowledge points after the attack.
   */
  public int specialPower(Building building) {
    delayCounter = 1;
    building.setImmune(true);
    return 0;
  }
}
