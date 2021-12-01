package bugs;

import java.lang.Math;

public abstract class Bug implements Comparable<Bug> {

  String name;
  int baseHp;
  int baseSteps;
  int level;

  int currentHp;
  int currentSteps;
  int currentFloor = -1;

  public Bug(String name, int baseHp, int baseSteps, int level, int initialSteps) {
    this.name = name;
    this.baseHp = baseHp;
    this.baseSteps = baseSteps;
    this.level = level;
    this.currentSteps = initialSteps;

    double dblLevel = level;
    double coefficient = 1.5;
    double tmpPower = Math.pow(dblLevel, coefficient);
    this.currentHp = (int) Math.round(baseHp * tmpPower);
  }

  public Bug(String name, int baseHp, int baseSteps, int level) {
    this.name = name;
    this.baseHp = baseHp;
    this.baseSteps = baseSteps;
    this.level = level;
  }

  public int getBaseSteps() {
    return baseSteps;
  }

  public int getLevel() {
    return level;
  }

  public Integer getCurrentHp() {
    Integer currentHpObject = Integer.valueOf(currentHp);
    return currentHp;
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
    currentHp = currentHp - damage;
    if (currentHp < 0) {
      currentHp = 0;
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
