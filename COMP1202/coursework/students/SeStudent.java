package students;

import java.lang.Math;
import building.*;
import bugs.*;
import java.util.ArrayList;

public class SeStudent implements Student {

	int level;
	int baseAtk = 5;
	int delay = 6;
	int delayCounter = 1;

	public SeStudent(int level) {
		this.level = level;
	}

	public int getLevel() {
		return level;
	}

	public int upgradeCost() {
		double dblLevel = level;
		return  100 * (int) Math.pow(2, level);
	}

	public void upgrade() {
		level += 1;
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
			Bug[] bugList = building.getAllBugs();

			int stopIndex = 4;
			if (stopIndex >= bugList.length) {
				stopIndex = bugList.length - 1;
			}

			for (int i = 0; i <= stopIndex; i++) {
				//System.out.println(bugList[i]);
				Bug bug = bugList[i];
				bug.slowDown(2);
			}
			return 0;
		}
	}
}