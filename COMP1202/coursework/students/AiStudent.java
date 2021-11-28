package students;

import java.lang.Math;
import building.*;
import bugs.*;
import java.util.ArrayList;

public class AiStudent implements Student {

  String name;
  int level;
  int baseAtk = 7;
  int delay = 6;
  int delayCounter = 1;

  public AiStudent(int level) {
    this.level = level;
  }

  public void setName(String name) {
    this.name = name;
  }

  public String getName() {
    return name;
  }

  public int getLevel() {
    return level;
  }

  public int upgradeCost() {
    double dblLevel = level;
    return  100 * (int) Math.pow(level, 2);
  }

  public int defence(Building building) {
    if (building.getAllBugsReal().size() == 0) {
      return 0;
    }

    if (delayCounter < delay) {
      delayCounter = delayCounter + 1;
      ArrayList<Bug> bugList = building.getAllBugs();
      if (bugList.size() == 0) {
        return 0;
      }

      Bug bug = bugList.get(0);
      double dblBaseAtk = baseAtk;
      bug.damage((int) Math.round(dblBaseAtk * Math.pow(level, 1.2)));
      if (bug.getCurrentHP() == 0) {
        building.removeBug(bug);
        return bug.getLevel() * 20;
      }
      return 0;
    } else {
      delayCounter = 1;
      System.out.println("Super attack!");
      ArrayList<Bug> bugList = building.getAllBugs();
      if (bugList.size() == 0) {
        return 0;
      }

      int totalKnowledgePts = 0;
      int stopIndex = 2;
      if (stopIndex >= bugList.size()) {
        stopIndex = bugList.size() - 1;
      }

      for (int i = 0; i <= stopIndex; i++) {
        Bug bug = bugList.get(i);
        double dblLevel = level;
        double dblBaseAtk = baseAtk;
        bug.damage((int) Math.round(dblBaseAtk * Math.pow(level, 1.2)));
        if (bug.getCurrentHP() == 0) {
          building.removeBug(bug);
          stopIndex = stopIndex - 1;
          i = i - 1;
          totalKnowledgePts = totalKnowledgePts + bug.getLevel() * 20;
        }
      }
      return totalKnowledgePts;
    }
  }
}
