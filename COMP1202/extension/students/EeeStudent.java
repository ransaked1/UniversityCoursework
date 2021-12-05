package students;

import bugs.*;
import building.*;

import java.util.ArrayList;
import java.lang.Math;
import java.util.Random;

/**
 * EeeStudent class that implements the Student interface and extends the AbstractStudent class with
 * functions for calculating the damage on level up and defence of the building. Its special power
 * generates a random multiplier between 1 and 8 realtively frequently.
 */
public class EeeStudent extends AbstractStudent implements Student {

  /**
   * Constructor that sets the student base attack, delay and level.
   *
   * @param level The level of the student to create.
   */
  public EeeStudent(int level) {
    super(5, 4, level);
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
      return this.getDamage() * 3; // About x3 multiplier on average
    }
  }

  /**
   * Defending the building by attacking the top bug or attacking with a random multiplier.
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
      return specialPower(building, totalKnowledgePts, bug); // Special power
    }
  }

  /**
   * Generates a random multiplier for damage between 1 and 8.
   *
   * @param building The builidng to apply immunity to.
   * @param totalKnowledgePts The knowledge points gained so far.
   * @param bug The bug to attack.
   * @return The total knowledge points after the attack.
   */
  public int specialPower(Building building, int totalKnowledgePts, Bug bug) {
    delayCounter = 1;
    // Generate a random integer between 0 and 99
    int randomInteger = new Random().nextInt(100);
    int multiplier;

    // 30, 20, 15, 15, 10, 5, 3, 2 percent chances for multipliers
    if (randomInteger <= 29) {
      multiplier = 1;
    } else if (randomInteger > 29 && randomInteger <= 49) {
      multiplier = 2;
    } else if (randomInteger > 49 && randomInteger <= 64) {
      multiplier = 3;
    } else if (randomInteger > 64 && randomInteger <= 79) {
      multiplier = 4;
    } else if (randomInteger > 79 && randomInteger <= 89) {
      multiplier = 5;
    } else if (randomInteger > 89 && randomInteger <= 94) {
      multiplier = 6;
    } else if (randomInteger > 94 && randomInteger <= 97) {
      multiplier = 7;
    } else {
      multiplier = 8;
    }
    return damageBug(building, totalKnowledgePts, bug, multiplier);
  }
}
