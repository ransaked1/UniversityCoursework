package students;

import building.*;
import bugs.*;

import java.lang.Math;
import java.util.ArrayList;

public class SeStudent extends AbstractStudent implements Student {

	public SeStudent(int level) {
		super(5, 6, level);
	}

	public int levelUpDamage() {
		if (delayCounter + 1 != delay) {
			double dblBaseAtk = baseAtk;
			return (int) Math.round(dblBaseAtk * Math.pow(level + 1, 1.2));
		} else {
			return 0;
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
			delayCounter = delayCounter + 1;
			return damageBug(building, totalKnowledgePts, bug, 1);
		} else {
			delayCounter = 1;
			return bugsStepBackSuper(bugList);
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

	private int bugsStepBackSuper(Bug[] bugList) {
		int stopIndex = 4;
		if (stopIndex >= bugList.length) {
			stopIndex = bugList.length - 1;
		}

		for (int i = 0; i <= stopIndex; i++) {
			Bug bug = bugList[i];
			bug.slowDown(2);
		}
		return 0;
	}
}