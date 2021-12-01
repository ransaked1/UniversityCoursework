package students;

import java.lang.Math;
import building.*;
import bugs.*;
import java.util.ArrayList;
import java.util.Random;

public class CyberStudent implements Student {

  int level;
  int baseAtk = 7;
  int delay = 8;
  int delayCounter = 1;

  public CyberStudent(int level) {
    this.level = level;
  }

  public int getLevel() {
    return level;
  }

  public int upgradeCost() {
    double dblLevel = level;
    return 100 * (int) Math.pow(2, level);
  }

  public int defence(Building building) {
    int totalKnowledgePts = 0;

    if (building.getAllBugs().length == 0) {
      return 0;
    }

    if (delayCounter < delay) {
      delayCounter = delayCounter + 1;
      Bug[] bugList = building.getAllBugs();
      Bug bug = bugList[0];

      double dblBaseAtk = baseAtk;
      bug.damage((int) Math.round(dblBaseAtk * Math.pow(level, 1.2)));
      if (bug.getCurrentHp() == 0) {
        totalKnowledgePts = totalKnowledgePts + bug.getLevel() * 20;
        building.removeBug(bug);
      }
      return totalKnowledgePts;
    } else {
      delayCounter = 1;
      System.out.println("Super attack!");
      int randomInteger = new Random().nextInt(100);
      Bug[] bugList = building.getAllBugs();
      Bug bug = bugList[0];

      int probability = 20 + level;
      if (probability > 50) {
        probability = 50;
      }

      System.out.println(randomInteger + " " + probability);

      if (randomInteger < probability) {
        totalKnowledgePts = totalKnowledgePts + bug.getLevel() * 20;
        building.removeBug(bug);
      } else {
        double dblBaseAtk = baseAtk * 2;
        bug.damage((int) Math.round(dblBaseAtk * Math.pow(level, 1.2)));
        if (bug.getCurrentHp() == 0) {
          totalKnowledgePts = totalKnowledgePts + bug.getLevel() * 20;
          building.removeBug(bug);
        }
      }
      return totalKnowledgePts;
    }
  }
}
