package bugs;

import java.lang.Math;

/**
 * Abstract class providing a Bug constructor, getters and setters, movement function, damage
 * function, slowdown and implementing a Comparable object for later sorting by floor and steps.
 */
public abstract class Bug implements Comparable<Bug> {

  private String name;
  private int baseHp;
  private int baseSteps;
  private int level;

  private int currentHp;
  private int currentSteps;
  private int currentFloor = -1;

  /**
   * Bug constructor that provides the initial steps.
   *
   * @param name
   * @param baseHp
   * @param baseSteps
   * @param level
   * @param initialSteps The number of steps before entering the building.
   */
  public Bug(String name, int baseHp, int baseSteps, int level, int initialSteps) {
    this.name = name;
    this.baseHp = baseHp;
    this.baseSteps = baseSteps;
    this.level = level;
    this.currentSteps = initialSteps;

    // Calculating the HP of the bug with the formula: Round(baseHP * (level^1.5))
    double dblLevel = level;
    double coefficient = 1.5;
    double tmpPower = Math.pow(dblLevel, coefficient);
    this.currentHp = (int) Math.round(baseHp * tmpPower);
  }

  /**
   * Bug constructor with no initial steps outside the building.
   *
   * @param name
   * @param baseHp
   * @param baseSteps
   * @param level
   */
  public Bug(String name, int baseHp, int baseSteps, int level) {
    this.name = name;
    this.baseHp = baseHp;
    this.baseSteps = baseSteps;
    this.level = level;
    this.currentSteps = 0;

    // Calculating the HP of the bug with the formula: Round(baseHP * (level^1.5))
    double dblLevel = level;
    double coefficient = 1.5;
    double tmpPower = Math.pow(dblLevel, coefficient);
    this.currentHp = (int) Math.round(baseHp * tmpPower);
  }

  public int getBaseSteps() {
    return baseSteps;
  }

  public int getLevel() {
    return level;
  }

  public int getCurrentSteps() {
    return currentSteps;
  }

  public int getCurrentFloor() {
    return currentFloor;
  }

  public String getName() {
    return name;
  }

  public Integer getCurrentHp() {
    return currentHp;
  }

  /**
   * Movement function decreseasing the number of steps to the end of the floor or moving to the
   * next floor.
   */
  public void move() {
    if (currentSteps > 0) {
      currentSteps = currentSteps - 1;  // Move one step to the next floor
    } else {
      currentFloor = currentFloor + 1;  // Increase the floor when reaching step 0
      currentSteps = baseSteps - 1;     // Set the current steps to baseSteps - 1
    }
  }

  /**
   * Damage function that subtracts the damage from the current HP and/or sets it to zero.
   *
   * @param damage
   */
  public void damage(int damage) {
    currentHp = currentHp - damage; // Take the damage out of the bug's HP

    // Making sure the HP of the bug is non-negative
    if (currentHp < 0) {
      currentHp = 0;
    }
  }

  /**
   * Push back the bug x amount of steps.
   *
   * @param steps The number of steps to push back.
   */
  public void slowDown(int steps) {
    this.currentSteps =
        this.currentSteps + steps; // Increasing the step count moves the bug backwards
  }

  /**
   * Comparator method that compares the floors and steps to the end of the floor of two bugs.
   *
   * @param b The bug to compare with.
   * @return 0 if on same position, -1 if forward and 1 if behind the bug compared to.
   */
  @Override
  public int compareTo(Bug b) {
    if (this.currentFloor == b.currentFloor) {
      if (this.currentSteps == b.currentSteps) {
        return 0;
      } else if (this.currentSteps > b.currentSteps) {
        return 1; // If the bug is on a higher step count it is behind the other
      } else {
        return -1;
      }
    } else if (this.currentFloor > b.currentFloor) {
      return -1; // If the bug is on a higher floor it is ahead of the other
    } else {
      return 1;
    }
  }
}
