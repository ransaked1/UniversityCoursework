import building.*;
import bugs.*;
import students.*;

import java.lang.Math;

public class Decision implements Comparable<Decision> {

	int decisionCost;
	double decisionDamageIncrease;
	double overallDecisionWeight;
	Student studentToUpgrade;

	public Decision(Team team) {
		decisionCost = team.getNewStudentCost();
		decisionDamageIncrease = 6.25;
		overallDecisionWeight = decisionDamageIncrease / decisionCost * 100;
		studentToUpgrade = null;
	}

	public Decision(Student student) {
		studentToUpgrade = student;
		decisionCost = student.upgradeCost();
		decisionDamageIncrease = student.levelUpDamage() - student.getDamage();
		overallDecisionWeight = decisionDamageIncrease / decisionCost * 100;
	}

	public double getOverallDecisionWeight() {
		return overallDecisionWeight;
	}

	public int getDecisionCost() {
		return decisionCost;
	}

	public Student getStudentToUpgrade() {
		return studentToUpgrade;
	}

	@Override
  public int compareTo(Decision decision) {
    if (this.overallDecisionWeight == decision.overallDecisionWeight) {
      return 0;
    } else if (this.overallDecisionWeight > decision.overallDecisionWeight) {
      return -1;
    } else {
      return 1;
    }
	}
}