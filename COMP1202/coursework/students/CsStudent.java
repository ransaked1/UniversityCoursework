package students;

import java.lang.Math;
import building.*;
import bugs.*;
import java.util.ArrayList;

public class CsStudent implements Student {

	int level;
	int baseAtk = 6;
	int delay = 6;
	int delayCounter = 1;

	public CsStudent(int level) {
		this.level = level;
	}

	public int getLevel() {
		return level;
	}

	public int getDamage() {
		double dblBaseAtk = baseAtk;
		return (int) Math.round(dblBaseAtk * Math.pow(level, 1.2));
	}

	public int getDelay() {
		return delay;
	}

	public int getDelayCounter() {
		return delayCounter;
	}

	public int upgradeCost() {
		double dblLevel = level;
		return  100 * (int) Math.pow(2, level);
	}

	public int levelUpDamage() {
		if (delayCounter + 1 != delay) {
			double dblBaseAtk = baseAtk;
			return (int) Math.round(dblBaseAtk * Math.pow(level + 1, 1.2));
		} else {
			double dblBaseAtk = baseAtk;
			return (int) Math.round(dblBaseAtk * Math.pow(level + 1, 1.2)) * 4;
		}
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
			Bug[] bugList = building.getAllBugs();
			Bug bug = bugList[0];

			double dblBaseAtk = baseAtk * 4;
			bug.damage((int) Math.round(dblBaseAtk * Math.pow(level, 1.2)));
			if (bug.getCurrentHp() == 0) {
				totalKnowledgePts = totalKnowledgePts + bug.getLevel() * 20;
				building.removeBug(bug);
			}
			return totalKnowledgePts;
		}
	}
}