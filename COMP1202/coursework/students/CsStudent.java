package students;

import building.*;
import bugs.*;

import java.lang.Math;
import java.util.ArrayList;

public class CsStudent extends AbstractStudent implements Student {

	public CsStudent(int level) {
		super(6, 6, level);
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
			return damageBug(building, totalKnowledgePts, bug, 4);
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
}