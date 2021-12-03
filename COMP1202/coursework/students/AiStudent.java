package students;

import building.*;
import bugs.*;

import java.util.ArrayList;
import java.lang.Math;

public class AiStudent extends AbstractStudent implements Student{

  public AiStudent(int level) {
    super(7, 7, level);
  }

  public int levelUpDamage() {
    if (delayCounter + 1 != delay) {
      double dblBaseAtk = baseAtk;
      return (int) Math.round(dblBaseAtk * Math.pow(level + 1, 1.2));
    } else {
      double dblBaseAtk = baseAtk;
      return (int) Math.round(dblBaseAtk * Math.pow(level + 1, 1.2)) * 3;
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
      return damageBugSuper(building, totalKnowledgePts, bugList);
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

  private int damageBugSuper(Building building, int totalKnowledgePts, Bug[] bugList) {
    int stopIndex = 2;
    if (stopIndex >= bugList.length) {
      stopIndex = bugList.length - 1;
    }

    for (int i = 0; i <= stopIndex; i++) {
      Bug bug = bugList[i];
      totalKnowledgePts += damageBug(building, totalKnowledgePts, bug, 1);
    }
    return totalKnowledgePts;
  }
}
