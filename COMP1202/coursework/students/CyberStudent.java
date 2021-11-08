package students;

import java.lang.Math;
import building.*;
import bugs.*;
import java.util.ArrayList;

public class CyberStudent implements Student {

	int level;
	int baseAtk = 7;
	int delay = 8;
	int delayCounter = 0;

	public CyberStudent(int level) {
		this.level = level;
	}

	public int getLevel() {
		return level;
	}

	public int upgradeCost() {
		double dblLevel = level;
		return 100 * (int) Math.pow(level, 2);
	}

	public int defence(Building building) {
		return 0;
	}
}