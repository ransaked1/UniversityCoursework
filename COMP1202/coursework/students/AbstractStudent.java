package students;

public abstract class AbstractStudent {

	int level;
	int baseAtk;
	int delay;
	int delayCounter = 1;

	public AbstractStudent(int baseAtk, int delay, int level) {
		this.level = level;
		this.baseAtk = baseAtk;
		this.delay = delay;
	}

	public int getDamage() {
		double dblBaseAtk = baseAtk;
		return (int) Math.round(dblBaseAtk * Math.pow(level, 1.2));
	}

	public int getLevel() {
		return level;
	}

	public int getDelayCounter() {
		return delayCounter;
	}

	public int getDelay() {
		return delay;
	}

	public int upgradeCost() {
		double dblLevel = level;
		return  100 * (int) Math.pow(2, level);
	}

	public void upgrade() {
		level += 1;
	}
}