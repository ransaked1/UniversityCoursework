import building.*;
import bugs.*;
import students.*;

import java.util.ArrayList;
import java.util.Collections;

public class Battle {

  Building building;
  Team team;
  int attackNumber = 1;

  public Battle(Building building, Team team) {
    this.building = building;
    this.team = team;
  }

  public boolean step() {
    boolean gameNotEnded = true;
    if (building.getConstructionPoints() == 0 || building.getAllBugsReal().size() == 0) {
      gameNotEnded = false;
      return gameNotEnded;
    }
    manageTeam();
    building.bugsMove();
    team.studentsAct(building);
    printGameState(attackNumber);
    attackNumber++;
    return gameNotEnded;
  }

  private void manageTeam() {
    boolean NoDecisionMade = true;
    ArrayList<Decision> decisionRank = new ArrayList<Decision>();
    decisionRank.add(new Decision(team));
    for (Student student : team.getStudents()) {
      decisionRank.add(new Decision(student));
    }
    Collections.sort(decisionRank);

//    for (Decision decision : decisionRank) {
//      System.out.println(decision.getOverallDecisionWeight());
//    }

    for (Decision decision : decisionRank) {
      if (decision.getDecisionCost() <= team.getKnowledgePoints()) {
        if (decision.getStudentToUpgrade() == null) {
          try {
            NoDecisionMade = false;
            team.recruitNewStudent();
            break;
          } catch (Exception e) {
            System.out.println(e);
          }
        } else {
          try {
            NoDecisionMade = false;
            team.upgrade(decision.getStudentToUpgrade());
            break;
          } catch (Exception e) {
            System.out.println(e);
          }
        }
      }
    }

    if (NoDecisionMade) {
      //TO DO: if not enough money for decision calculate what to make money for.
    }
  }

  public void printGameState(int attackNumber) {
    System.out.println();
    System.out.println();
    if (attackNumber == 0) {
      System.out.println(
          "<-------------------------------------------------------"
              + "--------------------------------------------------------------->");
      System.out.println("INITIAL GAME STATE: ");
    } else {
      System.out.println(
          "<-------------------------------------------------------"
              + "--------------------------------------------------------------->");
      System.out.println("Attack number: " + attackNumber);
    }
    for (Bug bug : building.getAllBugsReal()) {
      System.out.println(
          "Bug type: "
              + bug.getClass().getSimpleName()
              + " Bug name: "
              + bug.getName()
              + " Bug current floor: "
              + bug.getCurrentFloor()
              + " Bug current step: "
              + bug.getCurrentSteps()
              + " Bug current Hp: "
              + bug.getCurrentHp());
    }
    System.out.println();
    for (Student student : team.getStudents()) {
      System.out.println(
          "Student: "
              + student.getClass().getSimpleName()
              + " Student level: "
              + student.getLevel()
              + " Student attack: "
              + student.getDamage()
              + " Student power-up delay: "
              + student.getDelay()
              + " Student power-up counter: "
              + student.getDelayCounter());
    }
    System.out.println("Team knowledge points: " + team.getKnowledgePoints());
    System.out.println("Recruitment cost: " + team.getNewStudentCost());
    System.out.println("Building construction points: " + building.getConstructionPoints());
    System.out.println(
        "<-------------------------------------------------------"
            + "--------------------------------------------------------------->");
    try {
      Thread.sleep(500);
    } catch (InterruptedException e) {

    }
  }

  public void addBugs(ArrayList<Bug> bugWave) {
    for (Bug bug : bugWave) {
      building.addBug(bug);
    }
  }
}
