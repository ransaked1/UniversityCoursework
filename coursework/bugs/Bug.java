package bugs;

import java.lang.Math;

public abstract class Bug implements Comparable<Bug> {

  String name;
  int baseHP;
  int baseSteps;
  int level;

  int currentHP;
  int currentSteps;
  int currentFloor = -1;

  public Bug(String name, int baseHP, int baseSteps, int level, int initialSteps) {
    this.name = name;
    this.baseHP = baseHP;
    this.baseSteps = baseSteps;
    this.level = level;
    this.currentSteps = initialSteps;

    double dblLevel = level;
    double coefficient = 1.5;
    double tmpPower = Math.pow(dblLevel, coefficient);
    this.currentHP = (int) Math.round(baseHP * tmpPower);
  }

  public Bug(String name, int baseHP, int baseSteps, int level) {
    this.name = name;
    this.baseHP = baseHP;
    this.baseSteps = baseSteps;
    this.level = level;
  }

  public int getBaseSteps() {
    return baseSteps;
  }

  public int getLevel() {
    return level;
  }

  public int getCurrentHP() {
    return currentHP;
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

  public void move() {
    if (currentSteps > 0) {
      currentSteps = currentSteps - 1;
    } else {
      currentFloor = currentFloor + 1;
      currentSteps = baseSteps - 1;
    }
  }

  public void damage(int damage) {
    currentHP = currentHP - damage;
    if (currentHP < 0) {
      currentHP = 0;
    }
  }

  public void slowDown(int steps) {
    this.currentSteps = this.currentSteps + steps;
  }

  @Override
  public int compareTo(Bug b) {
    if (this.currentFloor == b.currentFloor) {
      if (this.currentSteps == b.currentSteps) {
        return 0;
      } else if (this.currentSteps > b.currentSteps) {
        return 1;
      } else {
        return -1;
      }
    } else if (this.currentFloor > b.currentFloor) {
      return -1;
    } else {
      return 1;
    }
  }
}
