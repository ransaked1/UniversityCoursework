package students;

import building.*;
import bugs.*;
import java.util.ArrayList;
import java.util.Random;

public class Team {

	int knowledgePoints;
	int newStudentCost = 100;
	ArrayList<Student> students = new ArrayList<Student>();

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

	public int studentsAct(Building building) {
		for (Student student : students) {
			knowledgePoints += student.defence(building);
		}
		return knowledgePoints;
	}

	public void recruitNewStudent() throws Exception {
		if (knowledgePoints < newStudentCost) {
			throw new Exception("Not enough knowledge points to recruit a student!");
		} else {
			knowledgePoints -= newStudentCost;
			newStudentCost += 10;
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

	public void upgrade(Student student) throws Exception {
		if (knowledgePoints < student.upgradeCost()) {
			throw new Exception("Not enough knowledge points to upgrade student!");
		} else {
			knowledgePoints -= student.upgradeCost();
			student.upgrade();
		}
	}

}
