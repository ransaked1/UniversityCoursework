import bugs.*;
import building.*;
import students.*;

import java.lang.Math;

/**
 * Decision class that calculates the weight of a decision for maximizing the damage in the next
 * round. It is calculated by the formula: decisionWeight = damgeIncrease / decisionCost * 100. The
 * class is implementing a Comparable object for later sorting by decision weight.
 */
public class Decision implements Comparable<Decision> {

  int decisionCost; // Knowledge points cost of the decision
  double decisionDamageIncrease; // The increase in damage output after applying the decision
  double overallDecisionWeight;
  Student studentToUpgrade;

  /**
   * Constructor that calculates the decision weight of recruiting a new student.
   *
   * @param team The team to recruit the new student to.
   */
  public Decision(Team team) {
    decisionCost = team.getNewStudentCost();
    decisionDamageIncrease = 6.1; // Potential average damage after recruiting a new student
    overallDecisionWeight = decisionDamageIncrease / decisionCost * 100;
    studentToUpgrade = null; // No student is upgraded when recruiting one
  }

  /**
   * Constructor that calculates the decision weight of upgrading a student in the team.
   *
   * @param student The potential student to be upgraded.
   */
  public Decision(Student student) {
    decisionCost = student.upgradeCost();
    // Damage increase is the damage output after the upgrade minus the current damage
    decisionDamageIncrease = student.levelUpDamage() - student.getDamage();
    overallDecisionWeight = decisionDamageIncrease / decisionCost * 100;
    studentToUpgrade = student;
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

  /**
   * Comparator method that compares two decisions.
   *
   * @param decision The decision to compare to.
   * @return 0 if on same position, -1 if greater and 1 if lower than the bug compared to.
   */
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
