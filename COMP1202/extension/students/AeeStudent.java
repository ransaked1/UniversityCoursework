package students;

import bugs.*;
import building.*;

import java.lang.Math;
import java.util.ArrayList;
import java.util.Random;

/**
 * AeeStudent class that implements the Student interface and extends the AbstractStudent class with
 * functions for calculating the damage on level up and defence of the building. Its special power
 * has 30 + level percent chance to push the top bug out the building.
 */
public class AeeStudent extends AbstractStudent implements Student {

  /**
   * Constructor that sets the student base attack, delay and level.
   *
   * @param level The level of the student to create.
   */
  public AeeStudent(int level) {
    super(5, 8, level);
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
      return specialPower(bug); // Push bugs out the building
    }
  }

  /**
   * Helper function that pushes the top bug out the building.
   *
   * @param bug The bug to knock out.
   * @return 0 knowledge points earned.
   */
  public int specialPower(Bug bug) {
    delayCounter = 1;
    // Generate a random integer between 0 and 99
    int randomInteger = new Random().nextInt(100);

    // Calculate the probabily for pushing the bug out the building
    int probability = 30 + level;
    if (randomInteger < probability) {
      bug.pushOut();
    }
    return 0;
  }
}
