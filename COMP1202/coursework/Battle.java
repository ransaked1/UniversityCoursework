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

  public boolean step(int waveCount, int numberOfWaves) {
    boolean gameNotEnded = true;
    boolean noMoreConstructionPoints = building.getConstructionPoints() == 0;
    boolean noMoreBugs = waveCount == numberOfWaves && building.getAllBugsReal().size() == 0;
    boolean moreBugsInWave = building.getAllBugsReal().size() != 0;

    if (noMoreConstructionPoints || noMoreBugs) {
      gameNotEnded = false;
      return gameNotEnded;
    }

    manageTeam();
    building.bugsMove();
    team.studentsAct(building);

    if (moreBugsInWave || noMoreBugs) {
      printGameState(attackNumber);
    }

    attackNumber++;
    return gameNotEnded;
  }

  private void manageTeam() {
    ArrayList<Decision> decisionRank = new ArrayList<Decision>();
    decisionRank.add(new Decision(team));
    for (Student student : team.getStudents()) {
      decisionRank.add(new Decision(student));
    }
    Collections.sort(decisionRank);

    for (Decision decision : decisionRank) {
      if (makeDecision(decision)) break;
    }
  }

  private boolean makeDecision(Decision decision) {
    if (decision.getDecisionCost() <= team.getKnowledgePoints()) {
      if (decision.getStudentToUpgrade() == null) {
        try {
          team.recruitNewStudent();
          return true;
        } catch (Exception e) {
          System.out.println(e);
        }
      } else {
        try {
          team.upgrade(decision.getStudentToUpgrade());
          return true;
        } catch (Exception e) {
          System.out.println(e);
        }
      }
    }
    return false;
  }

  public void printGameState(int attackNumber) {
    System.out.println();
    System.out.println();

    printTurnNumber(attackNumber);
    printBugsState();

    System.out.println();

    printStudentsState();
    printGeneralInfo();
    try {
      Thread.sleep(500);
    } catch (InterruptedException e) {
      System.out.println(e);
    }
  }

  private void printTurnNumber(int attackNumber) {
    System.out.println(
        "<-------------------------------------------------------"
            + "--------------------------------------------------------------->");
    if (attackNumber == 0) {
      System.out.println("INITIAL GAME STATE: ");
    } else {
      System.out.println("Attack number: " + attackNumber);
    }
  }

  private void printBugsState() {
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
  }

  private void printStudentsState() {
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
  }

  private void printGeneralInfo() {
    System.out.println("Team knowledge points: " + team.getKnowledgePoints());
    System.out.println("Recruitment cost: " + team.getNewStudentCost());
    System.out.println("Building construction points: " + building.getConstructionPoints());
    System.out.println(
        "<-------------------------------------------------------"
            + "--------------------------------------------------------------->");
  }

  public void addBugs(ArrayList<Bug> bugWave) {
    for (Bug bug : bugWave) {
      building.addBug(bug);
    }
  }
}
