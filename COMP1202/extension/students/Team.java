package students;

import bugs.*;
import building.*;

import java.util.ArrayList;
import java.util.Random;

/**
 * Team class that manages the student team with an ArrayList and manages their recruitment and
 * upgrading.
 */
public class Team {

  int knowledgePoints;
  int newStudentCost = 100; // Initial price to recruit a student
  ArrayList<Student> students = new ArrayList<Student>(); // The list of students in the team

  /**
   * Constructor that takes the initail knowledge points of the team.
   *
   * @param knowledgePoints In the beginning.
   */
  public Team(int knowledgePoints) {
    this.knowledgePoints = knowledgePoints;
  }

  public int getNewStudentCost() {
    return newStudentCost;
  }

  public int getKnowledgePoints() {
    return knowledgePoints;
  }

  public ArrayList<Student> getStudents() {
    return students;
  }

  /**
   * Going thorugh each student that defends the building and accumulating the knowledge points.
   *
   * @param building The building to defend.
   * @return The knowledge points of the team after the defence.
   */
  public int studentsAct(Building building) {
    for (Student student : students) {
      knowledgePoints += student.defence(building);
    }
    return knowledgePoints;
  }

  /**
   * Function that recruits a random new student to the team.
   *
   * @throws Exception If there are not enough knowledge points to recruit a student.
   */
  public void recruitNewStudent() throws Exception {
    if (knowledgePoints < newStudentCost) {
      throw new Exception("Not enough knowledge points to recruit a student!");
    } else {
      knowledgePoints -= newStudentCost; // Subtract the student cost
      newStudentCost += 10; // Icrease the cost for the next student by 10

      // Generate a random number and check which type of student with 25% chance it generates, add
      // the new student to the team
      int randomInteger = new Random().nextInt(100);
      if (randomInteger < 25) {
        students.add(new AiStudent(1));
      } else if (randomInteger < 50) {
        students.add(new CsStudent(1));
      } else if (randomInteger < 75) {
        students.add(new SeStudent(1));
      } else {
        students.add(new CyberStudent(1));
      }
    }
  }

  /**
   * Upgrade a students level.
   *
   * @param student The student to upgrade.
   * @throws Exception If there are not enough knowledge points to upgrade the student.
   */
  public void upgrade(Student student) throws Exception {
    if (knowledgePoints < student.upgradeCost()) {
      throw new Exception("Not enough knowledge points to upgrade student!");
    } else {
      knowledgePoints -= student.upgradeCost(); // Subtract the student cost
      student.upgrade();
    }
  }
}
