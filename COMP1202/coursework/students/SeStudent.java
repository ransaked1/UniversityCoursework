package students;

import java.lang.Math;
import building.*;
import bugs.*;
import java.util.ArrayList;

public class SeStudent implements Student {

	int level;
	int baseAtk = 5;
	int delay = 6;
	int delayCounter = 0;

	public SeStudent(int level) {
		this.level = level;
	}

	public int getLevel() {
		return level;
	}

	public int upgradeCost() {
		double dblLevel = level;
		return  100 * (int) Math.pow(level, 2);
	}

	public int defence(Building building) {
		return 0;
	}
}