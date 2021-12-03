package students;

import building.*;
import bugs.*;

import java.lang.Math;
import java.util.ArrayList;
import java.util.Random;

public class CyberStudent extends AbstractStudent implements Student {

  public CyberStudent(int level) {
    super(7, 8, level);
  }

  public int levelUpDamage() {
    if (delayCounter + 1 != delay) {
      double dblBaseAtk = baseAtk;
      return (int) Math.round(dblBaseAtk * Math.pow(level + 1, 1.2));
    } else {
      double dblBaseAtk = baseAtk;
      return (int) Math.round(dblBaseAtk * Math.pow(level + 1, 1.2)) * 2;
    }
  }

  public int defence(Building building) {
    int totalKnowledgePts = 0;

    if (building.getAllBugs().length == 0) {
      return 0;
    }

    Bug[] bugList = building.getAllBugs();
    Bug bug = bugList[0];
    if (delayCounter < delay) {
      delayCounter += 1;
      return damageBug(building, totalKnowledgePts, bug, 1);
    } else {
      delayCounter = 1;
      return damageBugSuper(building, totalKnowledgePts, bug);
    }
  }

  private int damageBug(Building building, int totalKnowledgePts, Bug bug, int multiplier) {
    double dblBaseAtk = baseAtk;
    bug.damage((int) Math.round(dblBaseAtk * Math.pow(level, 1.2)) * multiplier);
    if (bug.getCurrentHp() == 0) {
      totalKnowledgePts += bug.getLevel() * 20;
      building.removeBug(bug);
    }
    return totalKnowledgePts;
  }

  private int damageBugSuper(Building building, int totalKnowledgePts, Bug bug) {
    int randomInteger = new Random().nextInt(100);

    int probability = 20 + level;
    if (probability > 50) {
      probability = 50;
    }

    if (randomInteger < probability) {
      building.removeBug(bug);
      return totalKnowledgePts + bug.getLevel() * 20;
    } else {
      return damageBug(building, totalKnowledgePts, bug, 2);
    }
  }
}
