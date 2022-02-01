import bugs.*;
import building.*;
import students.*;

import java.util.ArrayList;
import java.util.Collections;

/**
 * Battle class that manages a game between bugs and students defending a building. It manages the
 * turn order, adds bugs to the game and prints the game state.
 */
public class Battle {

  Building building;
  Team team;
  int roundNumber = 1; // Round count

  /**
   * Constructor that starts a game with a building and a student team.
   *
   * @param building The building object to use.
   * @param team The student object to use.
   */
  public Battle(Team team, Building building) {
    this.building = building;
    this.team = team;
  }

  /**
   * Add bugs to the game.
   *
   * @param bugWave The list of bugs in the wave to add to the game.
   */
  public void addBugs(ArrayList<Bug> bugWave) {
    for (Bug bug : bugWave) {
      building.addBug(bug);
    }
  }

  /**
   * Method that processes one step of the simulation.
   *
   * @param waveCount Wave count so far.
   * @param numberOfWaves Total wave count for the game.
   * @return A boolean indicating the end of the game.
   */
  public boolean step(int waveCount, int numberOfWaves) {
    boolean gameNotEnded = true; // Initialize the game state as running

    // Initialized logic tests for later use
    boolean noMoreConstructionPoints = building.getConstructionPoints() == 0;
    boolean noMoreBugs = waveCount == numberOfWaves && building.getAllBugsReal().size() == 0;
    boolean moreBugsInWave = building.getAllBugsReal().size() != 0;

    // Stop the game if there are no more bugs (all waves completed) or the construction points
    // reach 0
    if (noMoreConstructionPoints || noMoreBugs) {
      gameNotEnded = false;
      return gameNotEnded;
    }

    // Manage the team (recruit/upgrade students), move the bugs a step forward, students defend the
    // building
    manageTeam();
    building.bugsMove();
    team.studentsAct(building);

    // Print the game state if there are bugs in the game and at the end when there are no bugs
    if (moreBugsInWave || noMoreBugs) {
      printGameState(roundNumber);
    }

    roundNumber++; // Increment the round number
    return gameNotEnded;
  }

  /**
   * Method that decides how to improve the team. My implementation focuses solely on achieving the
   * highest damage possible as soon as possible using the least amount of knowledge points by
   * weighting the potetial of each purchase by the cost for each point of damage to be dealt in the
   * next round.
   */
  private void manageTeam() {
    // Initialize a decision ranking list, implementation for Decision object found in Decision.java
    ArrayList<Decision> decisionRank = new ArrayList<Decision>();
    decisionRank.add(new Decision(team)); // Create decision for recruiting a new student

    // Create a decision for each current student upgrade
    for (Student student : team.getStudents()) {
      decisionRank.add(new Decision(student));
    }
    Collections.sort(decisionRank); // Sort by the highest weight (damage possible)

    // Take the highest weight decision that we can afford
    for (Decision decision : decisionRank) {
      if (makeDecision(decision)) {
        break;
      }
    }
  }

  /**
   * Printing the game state initially and each round.
   *
   * @param roundNumber The current round number.
   */
  public void printGameState(int roundNumber) {
    System.out.println();
    System.out.println();

    printRoundNumber(roundNumber); // Formatting the round number print
    printBugsState(); // Printing the bugs in the game

    System.out.println();

    printStudentsState(); // Printing the students on the team
    printGeneralInfo();

    // Make a 0.5 second pause between rounds
    try {
      Thread.sleep(500);
    } catch (InterruptedException e) {
      System.out.println(e);
    }
  }

  /**
   * Helper method that applies the decision made by either recruiting a student or by upgrading
   * one.
   *
   * @param decision The decision selected by the manager.
   * @return A boolean telling if we can afford the decision.
   */
  private boolean makeDecision(Decision decision) {
    // Check that the decision can be afforded
    if (decision.getDecisionCost() <= team.getKnowledgePoints()) {
      // If there is no student to upgrade recruit a student, else upgrade the student in decision
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

  /**
   * Helper method printing the round number.
   *
   * @param roundNumber The number of the current round.
   */
  private void printRoundNumber(int roundNumber) {
    System.out.println(
        "<-------------------------------------------------------"
            + "--------------------------------------------------------------->");
    if (roundNumber == 0) {
      System.out.println("INITIAL GAME STATE: ");
    } else {
      System.out.println("Round number: " + roundNumber);
    }
  }

  /**
   * Helper method that prints the type, name, floor, step and HP of each bug currently in the game.
   */
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

  /**
   * Helper method that prints the type, level, damage, power-up dealy and counter of each student
   * currently in the team.
   */
  private void printStudentsState() {
    for (Student student : team.getStudents()) {
      System.out.println(
          "Student: "
              + student.getClass().getSimpleName()
              + " Student level: "
              + student.getLevel()
              + " Student damage: "
              + student.getDamage()
              + " Student power-up delay: "
              + student.getDelay()
              + " Student power-up counter: "
              + student.getDelayCounter());
    }
  }

  /**
   * Helper method that prints the general information of the game: knowledge points of the team,
   * recruitment cost of a new student and the building's current construction points.
   */
  private void printGeneralInfo() {
    System.out.println("Team knowledge points: " + team.getKnowledgePoints());
    System.out.println("Recruitment cost: " + team.getNewStudentCost());
    System.out.println("Building construction points: " + building.getConstructionPoints());
    System.out.println(
        "<-------------------------------------------------------"
            + "--------------------------------------------------------------->");
  }
}
