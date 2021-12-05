package students;

import bugs.*;
import building.*;

import java.util.ArrayList;
import java.lang.Math;
import java.util.Random;

/**
 * BeStudent class that implements the Student interface and extends the AbstractStudent class with
 * functions for calculating the damage on level up and defence of the building. Its special power
 * has 5 + level percent chance of upgrading the student with no cost or deals 2x damage.
 */
public class BeStudent extends AbstractStudent implements Student {

  /**
   * Constructor that sets the student base attack, delay and level.
   *
   * @param level The level of the student to create.
   */
  public BeStudent(int level) {
    super(4, 10, level);
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
      return this.getDamage() * 2; // The damage to the top bug
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
      return specialPower(building, totalKnowledgePts, bug); // Special attack
    }
  }

  /**
   * Helper method that applies a special attack to a bug and removes it if it's killed. The special
   * power upgrades the student for no cost or deals 2x the damage.
   *
   * @param building The building object to defend.
   * @param totalKnowledgePts The knowledge points gained so far.
   * @param bug The bug to attack.
   * @return The total knowledge points after the attack.
   */
  public int specialPower(Building building, int totalKnowledgePts, Bug bug) {
    delayCounter = 1;
    // Generate a random integer between 0 and 99
    int randomInteger = new Random().nextInt(100);

    // Calculate the probabily for a free upgrade
    int probability = 5 + level;

    // If the number falls under the probability range upgrade otherwise apply 2x standard damage
    if (randomInteger < probability) {
      upgrade();
      return 0;
    } else {
      return damageBug(building, totalKnowledgePts, bug, 2);
    }
  }
}
