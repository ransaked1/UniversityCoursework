package students;

import bugs.*;
import building.*;

import java.lang.Math;

/**
 * Abstract Student class that implements the common functionality for all types of students like
 * getters, the damage they can deal, upgrade cost method, the upgrade method and the calculator
 * method for the standard damage they can deal to a bug.
 */
public abstract class AbstractStudent {

  int level;
  int baseAtk;
  int delay;
  int delayCounter = 1; // Counter until special attack is triggered

  /**
   * Constructor for the abstract class that needs the student leve, base damage and delay until the
   * next special attack.
   *
   * @param baseAtk Base attack of the student.
   * @param delay Steps until special attack.
   * @param level Level of the student.
   */
  public AbstractStudent(int baseAtk, int delay, int level) {
    this.level = level;
    this.baseAtk = baseAtk;
    this.delay = delay;
  }

  public int getLevel() {
    return level;
  }

  public int getDelayCounter() {
    return delayCounter;
  }

  public int getDelay() {
    return delay;
  }

  /**
   * Calculates the upgrade cost of a student based on its level by the formula: 100 * (2 ^ level).
   *
   * @return The upgrade cost for the student.
   */
  public int upgradeCost() {
    double dblLevel = level;
    return 100 * (int) Math.pow(2, level);
  }

  /** Increment the student's level. */
  public void upgrade() {
    level += 1;
  }

  /**
   * Applies a standard attack to a bug and removes it if it's killed.
   *
   * @param building The builidng to remove the bug from.
   * @param totalKnowledgePts The knowledge points gained so far.
   * @param bug The bug to attack.
   * @param multiplier The attack multiplier for this attack.
   * @return The total knowledge points after the attack.
   */
  public int damageBug(Building building, int totalKnowledgePts, Bug bug, int multiplier) {
    double dblBaseAtk = baseAtk;
    bug.damage((int) Math.round(this.getDamage() * multiplier)); // Apply damage
    if (bug.getCurrentHp() == 0) {
      // Increase the knowledge points by bugLevel * 20 points and remove the bug from the building
      totalKnowledgePts += bug.getLevel() * 20;
      building.removeBug(bug);
    }
    return totalKnowledgePts;
  }

  /**
   * Get the damage the student can deal. Calculated by the formula: Round(baseAtk * (level^1.2)).
   *
   * @return The damage the student can deal as a rounded integer.
   */
  public int getDamage() {
    double dblBaseAtk = baseAtk;
    return (int) Math.round(dblBaseAtk * Math.pow(level, 1.2));
  }
}
