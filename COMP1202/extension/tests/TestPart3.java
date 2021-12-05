/***************************************************************************************************
 * Copyright (c) 2020,2021 University of Southampton.
 *
 * This program and the accompanying materials are made available under the terms of the Eclipse
 * Public License 2.0 which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
 *
 * Contributors:
 *     University of Southampton - initial API and implementation
 **************************************************************************************************/

import static java.time.Duration.ofMillis;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTimeout;
import static org.junit.jupiter.api.Assertions.fail;

import java.lang.reflect.Array;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

public class TestPart3 extends TestCoursework {

  /**
   * Test class for {@link students.Student}'s signature.
   *
   * @author htson
   * @version 1.0
   * @see TestCoursework.AbstractTestStudent
   */
  @Nested
  public class TestStudentSignature extends AbstractTestStudent {

    private Class<?> buildingClass;

    @BeforeEach
    public void setup() {
      super.setup();
      buildingClass = getClassOrInterface("building.Building");
    }

    @Test
    @DisplayName("Test students.Student's signature")
    public void testStudent_Signature() {
      assertInterface("");
      assertMethod("", "getLevel");
      assertMethod("", "upgradeCost");
      assertMethod("", "defence", buildingClass);
    }
  }

  /**
   * Test class for {@link students.AiStudent}'s signature.
   *
   * @author htson
   * @version 1.0
   * @see AbstractTestAiStudent
   */
  @Nested
  public class TestAiStudentSignature extends AbstractTestAiStudent {

    private Class<?> buildingClass;

    @BeforeEach
    public void setup() {
      super.setup();
      buildingClass = getClassOrInterface("building.Building");
    }

    @Test
    @DisplayName("Test students.AiStudent's signature")
    public void testAiStudent_Signature() {
      assertClass("");
      assertConstructor("", int.class);
      assertMethod("", "getLevel");
      assertMethod("", "upgradeCost");
      assertMethod("", "defence", buildingClass);
    }
  }

  /**
   * Test class for {@link students.AiStudent}'s specification.
   *
   * @author htson
   * @version 1.0
   * @see TestCoursework.AbstractTestAiStudent
   */
  @Nested
  public class TestAiStudentSpecification extends AbstractTestAiStudent {

    private Class<?> bugClass;
    private Class<?> buildingClass;
    private Class<?> cmClass;
    private Class<?> ntClass;
    private Class<?> npClass;
    private Class<?> bugArrayClass;

    @BeforeEach
    public void setup() {
      super.setup();
      bugClass = getClassOrInterface("bugs.Bug");
      buildingClass = getClassOrInterface("building.Building");
      cmClass = getClassOrInterface("bugs.ConcurrentModificationBug");
      cmClass = getClassOrInterface("bugs.ConcurrentModificationBug");
      ntClass = getClassOrInterface("bugs.NoneTerminationBug");
      npClass = getClassOrInterface("bugs.NullPointerBug");
      bugArrayClass = Array.newInstance(bugClass, 0).getClass();
    }

    @Test
    @DisplayName("Test students.AiStudent's constructor and getters")
    public void testAiStudent_constructor() {
      assertTimeout(ofMillis(1000), () -> {
        Constructor<?> aiStudentConstructor = assertConstructor("", int.class);
        Method getLevelMethod = assertMethod("", "getLevel");
        Method getUpgradeCostMethod = assertMethod("", "upgradeCost");
        Object aiStudent;
        try {
          aiStudent = aiStudentConstructor.newInstance(1);
        } catch (InstantiationException | IllegalAccessException | IllegalArgumentException
            | InvocationTargetException e) {
          e.printStackTrace();
          fail(": Setting up failed");
          return;
        }
        try {
          assertEquals(1, getLevelMethod.invoke(aiStudent), ": Incorrect level for AiStudent");
          assertEquals(200, getUpgradeCostMethod.invoke(aiStudent),
              ": Incorrect upgrade cost for AiStudent Level 1");
        } catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
          e.printStackTrace();
          fail(": Running test failed");
        }

      });
    }

    @Test
    @DisplayName("Test students.AiStudent's defence method")
    public void testAiStudent_defense() {
      assertTimeout(ofMillis(1000), () -> {
        Object aiStudent;
        Object building;
        Object cmBug1;
        Object ntBug1;
        Object npBug1;
        Object cmBug2;
        Object ntBug2;
        Object npBug2;
        Method defenceMethod;
        Method getAllBugsMethod;
        Method getCurrentHpMethod;
        try {
          Constructor<?> aiStudentConstructor = assertConstructor("", int.class);
          defenceMethod = assertAccessibleMethod("", "defence", buildingClass);
          Constructor<?> buildingConstructor = assertConstructor(buildingClass, "", int.class,
              int.class);
          Method addBugMethod = assertMethod(buildingClass, "", "addBug", bugClass);
          Method bugsMoveMethod = assertMethod(buildingClass, "", "bugsMove");
          getAllBugsMethod = assertMethod(buildingClass, "", "getAllBugs");
          getCurrentHpMethod = assertMethod(bugClass, "", "getCurrentHp");
          Constructor<?> cmConstructor = assertConstructor(cmClass, "", String.class, int.class,
              int.class);
          Constructor<?> ntConstructor = assertConstructor(ntClass, "", String.class, int.class,
              int.class);
          Constructor<?> npConstructor = assertConstructor(npClass, "", String.class, int.class,
              int.class);
          building = buildingConstructor.newInstance(4, 5);
          cmBug1 = cmConstructor.newInstance("ConcurrentModificationBug1", 1, 0);
          ntBug1 = ntConstructor.newInstance("NoneTerminationBug1", 1, 0);
          npBug1 = npConstructor.newInstance("NullPointerBug1", 3, 0);
          cmBug2 = cmConstructor.newInstance("ConcurrentModificationBug2", 1, 3);
          ntBug2 = ntConstructor.newInstance("NoneTerminationBug2", 1, 2);
          npBug2 = npConstructor.newInstance("NullPointerBug2", 1, 4);
          aiStudent = aiStudentConstructor.newInstance(1);
          addBugMethod.invoke(building, cmBug1); // 20 HP
          addBugMethod.invoke(building, ntBug1); // 200 HP
          addBugMethod.invoke(building, npBug1); // 52 HP
          addBugMethod.invoke(building, cmBug2); // 20 HP
          addBugMethod.invoke(building, ntBug2); // 200 HP
          addBugMethod.invoke(building, npBug2); // 10 HP
          bugsMoveMethod.invoke(building); // Bug enters the building
          bugsMoveMethod.invoke(building); // Bug enters the building
          bugsMoveMethod.invoke(building); // Bug enters the building
          bugsMoveMethod.invoke(building); // Bug enters the building
        } catch (InstantiationException | IllegalAccessException | IllegalArgumentException
            | InvocationTargetException e) {
          e.printStackTrace();
          fail(": Setting up failed");
          return;
        }
        try {
          Object result;
          Object[] bugs;

          // Attack 1: causes 7 damages to npBug1 => [npBug1(45), cmBug1(20), ntBug1(200), cmBug2(20), ntBug2(200)] [npBug2(10)]
          assertEquals(0, defenceMethod.invoke(aiStudent, building),
              "There should be no knowledge points gained");
          assertEquals(45, getCurrentHpMethod.invoke(npBug1));
          assertEquals(20, getCurrentHpMethod.invoke(cmBug1));
          assertEquals(200, getCurrentHpMethod.invoke(ntBug1));
          assertEquals(10, getCurrentHpMethod.invoke(npBug2));
          assertEquals(20, getCurrentHpMethod.invoke(cmBug2));
          assertEquals(200, getCurrentHpMethod.invoke(ntBug2));
          result = getAllBugsMethod.invoke(building);
          if (!bugArrayClass.isInstance(result)) {
            fail("Expect to return an array of bugs");
          }
          bugs = (Object[]) result;
          assertEquals(5, bugs.length, ": There should be 5 bugs");
          assertEquals(npBug1, bugs[0], "The 1st bug should be NullPointerBug1");
          assertEquals(cmBug1, bugs[1], "The 2nd bug should be ConcurrentModificationBug1");
          assertEquals(ntBug1, bugs[2], "The 3rd bug should be NoneTerminationBug1");
          assertEquals(cmBug2, bugs[3], "The 4th bug should be ConcurrentModificationBug2");
          assertEquals(ntBug2, bugs[4], "The 5th bug should be NoneTerminationBug2");

          // Attack 2: causes 7 damages to npBug1 => [npBug1(38), cmBug1(20), ntBug1(200), cmBug2(20), ntBug2(200)] [npBug2(10)]
          assertEquals(0, defenceMethod.invoke(aiStudent, building),
              "There should be no knowledge points gained");
          assertEquals(38, getCurrentHpMethod.invoke(npBug1));
          assertEquals(20, getCurrentHpMethod.invoke(cmBug1));
          assertEquals(200, getCurrentHpMethod.invoke(ntBug1));
          assertEquals(10, getCurrentHpMethod.invoke(npBug2));
          assertEquals(20, getCurrentHpMethod.invoke(cmBug2));
          assertEquals(200, getCurrentHpMethod.invoke(ntBug2));
          result = getAllBugsMethod.invoke(building);
          if (!bugArrayClass.isInstance(result)) {
            fail("Expect to return an array of bugs");
          }
          bugs = (Object[]) result;
          assertEquals(5, bugs.length, ": There should be 5 bugs");
          assertEquals(npBug1, bugs[0], "The 1st bug should be NullPointerBug1");
          assertEquals(cmBug1, bugs[1], "The 2nd bug should be ConcurrentModificationBug1");
          assertEquals(ntBug1, bugs[2], "The 3rd bug should be NoneTerminationBug1");
          assertEquals(cmBug2, bugs[3], "The 4th bug should be ConcurrentModificationBug2");
          assertEquals(ntBug2, bugs[4], "The 5th bug should be NoneTerminationBug2");

          // Attack 3: causes 7 damages to npBug1 => [npBug1(31), cmBug1(20), ntBug1(200), cmBug2(20), ntBug2(200)] [npBug2(10)]
          assertEquals(0, defenceMethod.invoke(aiStudent, building),
              "There should be no knowledge points gained");
          assertEquals(31, getCurrentHpMethod.invoke(npBug1));
          assertEquals(20, getCurrentHpMethod.invoke(cmBug1));
          assertEquals(200, getCurrentHpMethod.invoke(ntBug1));
          assertEquals(10, getCurrentHpMethod.invoke(npBug2));
          assertEquals(20, getCurrentHpMethod.invoke(cmBug2));
          assertEquals(200, getCurrentHpMethod.invoke(ntBug2));
          result = getAllBugsMethod.invoke(building);
          if (!bugArrayClass.isInstance(result)) {
            fail("Expect to return an array of bugs");
          }
          bugs = (Object[]) result;
          assertEquals(5, bugs.length, ": There should be 5 bugs");
          assertEquals(npBug1, bugs[0], "The 1st bug should be NullPointerBug1");
          assertEquals(cmBug1, bugs[1], "The 2nd bug should be ConcurrentModificationBug1");
          assertEquals(ntBug1, bugs[2], "The 3rd bug should be NoneTerminationBug1");
          assertEquals(cmBug2, bugs[3], "The 4th bug should be ConcurrentModificationBug2");
          assertEquals(ntBug2, bugs[4], "The 5th bug should be NoneTerminationBug2");

          // Attack 4: causes 7 damages to npBug1 => [npBug1(24), cmBug1(20), ntBug1(200), cmBug2(20), ntBug2(200)] [npBug2(10)]
          assertEquals(0, defenceMethod.invoke(aiStudent, building),
              "There should be no knowledge points gained");
          assertEquals(24, getCurrentHpMethod.invoke(npBug1));
          assertEquals(20, getCurrentHpMethod.invoke(cmBug1));
          assertEquals(200, getCurrentHpMethod.invoke(ntBug1));
          assertEquals(10, getCurrentHpMethod.invoke(npBug2));
          assertEquals(20, getCurrentHpMethod.invoke(cmBug2));
          assertEquals(200, getCurrentHpMethod.invoke(ntBug2));
          result = getAllBugsMethod.invoke(building);
          if (!bugArrayClass.isInstance(result)) {
            fail("Expect to return an array of bugs");
          }
          bugs = (Object[]) result;
          assertEquals(5, bugs.length, ": There should be 5 bugs");
          assertEquals(npBug1, bugs[0], "The 1st bug should be NullPointerBug1");
          assertEquals(cmBug1, bugs[1], "The 2nd bug should be ConcurrentModificationBug1");
          assertEquals(ntBug1, bugs[2], "The 3rd bug should be NoneTerminationBug1");
          assertEquals(cmBug2, bugs[3], "The 4th bug should be ConcurrentModificationBug2");
          assertEquals(ntBug2, bugs[4], "The 5th bug should be NoneTerminationBug2");

          // Attack 5: causes 7 damages to npBug1 => [npBug1(17), cmBug1(20), ntBug1(200), cmBug2(20), ntBug2(200)] [npBug2(10)]
          assertEquals(0, defenceMethod.invoke(aiStudent, building),
              "There should be no knowledge points gained");
          assertEquals(17, getCurrentHpMethod.invoke(npBug1));
          assertEquals(20, getCurrentHpMethod.invoke(cmBug1));
          assertEquals(200, getCurrentHpMethod.invoke(ntBug1));
          assertEquals(10, getCurrentHpMethod.invoke(npBug2));
          assertEquals(20, getCurrentHpMethod.invoke(cmBug2));
          assertEquals(200, getCurrentHpMethod.invoke(ntBug2));
          result = getAllBugsMethod.invoke(building);
          if (!bugArrayClass.isInstance(result)) {
            fail("Expect to return an array of bugs");
          }
          bugs = (Object[]) result;
          assertEquals(5, bugs.length, ": There should be 5 bugs");
          assertEquals(npBug1, bugs[0], "The 1st bug should be NullPointerBug1");
          assertEquals(cmBug1, bugs[1], "The 2nd bug should be ConcurrentModificationBug1");
          assertEquals(ntBug1, bugs[2], "The 3rd bug should be NoneTerminationBug1");
          assertEquals(cmBug2, bugs[3], "The 4th bug should be ConcurrentModificationBug2");
          assertEquals(ntBug2, bugs[4], "The 5th bug should be NoneTerminationBug2");

          // Attack 6 causes 7 damages to npBug1 => [npBug1(10), cmBug1(20), ntBug1(200), cmBug2(20), ntBug2(200)] [npBug2(10)]
          assertEquals(0, defenceMethod.invoke(aiStudent, building),
              "There should be no knowledge points gained");
          assertEquals(10, getCurrentHpMethod.invoke(npBug1));
          assertEquals(20, getCurrentHpMethod.invoke(cmBug1));
          assertEquals(200, getCurrentHpMethod.invoke(ntBug1));
          assertEquals(10, getCurrentHpMethod.invoke(npBug2));
          assertEquals(20, getCurrentHpMethod.invoke(cmBug2));
          assertEquals(200, getCurrentHpMethod.invoke(ntBug2));
          result = getAllBugsMethod.invoke(building);
          if (!bugArrayClass.isInstance(result)) {
            fail("Expect to return an array of bugs");
          }
          bugs = (Object[]) result;
          assertEquals(5, bugs.length, ": There should be 5 bugs");
          assertEquals(npBug1, bugs[0], "The 1st bug should be NullPointerBug1");
          assertEquals(cmBug1, bugs[1], "The 2nd bug should be ConcurrentModificationBug1");
          assertEquals(ntBug1, bugs[2], "The 3rd bug should be NoneTerminationBug1");
          assertEquals(cmBug2, bugs[3], "The 4th bug should be ConcurrentModificationBug2");
          assertEquals(ntBug2, bugs[4], "The 5th bug should be NoneTerminationBug2");

          // Attack 7 causes 7 damages to npBug1, cmBug1, ntBug1 => [npBug1(3), cmBug1(13), ntBug1(193), cmBug2(20), ntBug2(200)] [npBug2(10)]
          assertEquals(0, defenceMethod.invoke(aiStudent, building),
              "There should be no knowledge points gained");
          assertEquals(3, getCurrentHpMethod.invoke(npBug1));
          assertEquals(13, getCurrentHpMethod.invoke(cmBug1));
          assertEquals(193, getCurrentHpMethod.invoke(ntBug1));
          assertEquals(10, getCurrentHpMethod.invoke(npBug2));
          assertEquals(20, getCurrentHpMethod.invoke(cmBug2));
          assertEquals(200, getCurrentHpMethod.invoke(ntBug2));
          result = getAllBugsMethod.invoke(building);
          if (!bugArrayClass.isInstance(result)) {
            fail("Expect to return an array of bugs");
          }
          bugs = (Object[]) result;
          assertEquals(5, bugs.length, ": There should be 5 bugs");
          assertEquals(npBug1, bugs[0], "The 1st bug should be NullPointerBug1");
          assertEquals(cmBug1, bugs[1], "The 2nd bug should be ConcurrentModificationBug1");
          assertEquals(ntBug1, bugs[2], "The 3rd bug should be NoneTerminationBug1");
          assertEquals(cmBug2, bugs[3], "The 4th bug should be ConcurrentModificationBug2");
          assertEquals(ntBug2, bugs[4], "The 5th bug should be NoneTerminationBug2");

          // Attack 8 causes 7 damages and remove npBug1 => [cmBug1(13), ntBug1(193), cmBug2(20), ntBug2(200)] [npBug2(10)]
          assertEquals(60, defenceMethod.invoke(aiStudent, building),
              "There should be 60 knowledge points gained");
          assertEquals(0, getCurrentHpMethod.invoke(npBug1));
          assertEquals(13, getCurrentHpMethod.invoke(cmBug1));
          assertEquals(193, getCurrentHpMethod.invoke(ntBug1));
          assertEquals(10, getCurrentHpMethod.invoke(npBug2));
          assertEquals(20, getCurrentHpMethod.invoke(cmBug2));
          assertEquals(200, getCurrentHpMethod.invoke(ntBug2));
          result = getAllBugsMethod.invoke(building);
          if (!bugArrayClass.isInstance(result)) {
            fail("Expect to return an array of bugs");
          }
          bugs = (Object[]) result;
          assertEquals(4, bugs.length, ": There should be 4 bugs");
          assertEquals(cmBug1, bugs[0], "The 1st bug should be ConcurrentModificationBug1");
          assertEquals(ntBug1, bugs[1], "The 2nd bug should be NoneTerminationBug1");
          assertEquals(cmBug2, bugs[2], "The 3rd bug should be ConcurrentModificationBug2");
          assertEquals(ntBug2, bugs[3], "The 4th bug should be NoneTerminationBug2");
        } catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
          e.printStackTrace();
          fail(": Running test failed");
        }

      });
    }

  }

  /**
   * Test class for {@link students.CsStudent}'s signature.
   *
   * @author htson
   * @version 1.0
   * @see AbstractTestCsStudent
   */
  @Nested
  public class TestCsStudentSignature extends AbstractTestCsStudent {

    private Class<?> buildingClass;

    @BeforeEach
    public void setup() {
      super.setup();
      buildingClass = getClassOrInterface("building.Building");
    }

    @Test
    @DisplayName("Test students.CsStudent's signature")
    public void testCsStudent_Signature() {
      assertClass("");
      assertConstructor("", int.class);
      assertMethod("", "getLevel");
      assertMethod("", "upgradeCost");
      assertMethod("", "defence", buildingClass);
    }
  }

  /**
   * Test class for {@link students.CsStudent}'s specification.
   *
   * @author htson
   * @version 1.0
   * @see TestCoursework.AbstractTestCsStudent
   */
  @Nested
  public class TestCsStudentSpecification extends AbstractTestCsStudent {

    private Class<?> bugClass;
    private Class<?> buildingClass;
    private Class<?> cmClass;
    private Class<?> ntClass;
    private Class<?> npClass;
    private Class<?> bugArrayClass;

    @BeforeEach
    public void setup() {
      super.setup();
      bugClass = getClassOrInterface("bugs.Bug");
      buildingClass = getClassOrInterface("building.Building");
      cmClass = getClassOrInterface("bugs.ConcurrentModificationBug");
      cmClass = getClassOrInterface("bugs.ConcurrentModificationBug");
      ntClass = getClassOrInterface("bugs.NoneTerminationBug");
      npClass = getClassOrInterface("bugs.NullPointerBug");
      bugArrayClass = Array.newInstance(bugClass, 0).getClass();
    }

    @Test
    @DisplayName("Test students.CsStudent's constructor and getters")
    public void testCsStudent_constructor() {
      assertTimeout(ofMillis(1000), () -> {
        Constructor<?> csStudentConstructor = assertConstructor("", int.class);
        Method getLevelMethod = assertMethod("", "getLevel");
        Method getUpgradeCostMethod = assertMethod("", "upgradeCost");
        Object csStudent;
        try {
          csStudent = csStudentConstructor.newInstance(1);
        } catch (InstantiationException | IllegalAccessException | IllegalArgumentException
            | InvocationTargetException e) {
          e.printStackTrace();
          fail(": Setting up failed");
          return;
        }
        try {
          assertEquals(1, getLevelMethod.invoke(csStudent), ": Incorrect level for CsStudent");
          assertEquals(200, getUpgradeCostMethod.invoke(csStudent),
              ": Incorrect upgrade cost for CsStudent Level 1");
        } catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
          e.printStackTrace();
          fail(": Running test failed");
        }

      });
    }

    @Test
    @DisplayName("Test students.CsStudent's defence method")
    public void testCsStudent_defense() {
      assertTimeout(ofMillis(1000), () -> {
        Object csStudent;
        Object building;
        Object cmBug1;
        Object ntBug1;
        Object npBug1;
        Object cmBug2;
        Object ntBug2;
        Object npBug2;
        Method defenceMethod;
        Method getAllBugsMethod;
        Method getCurrentHpMethod;
        try {
          Constructor<?> csStudentConstructor = assertConstructor("", int.class);
          defenceMethod = assertAccessibleMethod("", "defence", buildingClass);
          Constructor<?> buildingConstructor = assertConstructor(buildingClass, "", int.class,
              int.class);
          Method addBugMethod = assertMethod(buildingClass, "", "addBug", bugClass);
          Method bugsMoveMethod = assertMethod(buildingClass, "", "bugsMove");
          getAllBugsMethod = assertMethod(buildingClass, "", "getAllBugs");
          getCurrentHpMethod = assertMethod(bugClass, "", "getCurrentHp");
          Constructor<?> cmConstructor = assertConstructor(cmClass, "", String.class, int.class,
              int.class);
          Constructor<?> ntConstructor = assertConstructor(ntClass, "", String.class, int.class,
              int.class);
          Constructor<?> npConstructor = assertConstructor(npClass, "", String.class, int.class,
              int.class);
          building = buildingConstructor.newInstance(4, 5);
          cmBug1 = cmConstructor.newInstance("ConcurrentModificationBug1", 2, 0);
          ntBug1 = ntConstructor.newInstance("NoneTerminationBug1", 1, 0);
          npBug1 = npConstructor.newInstance("NullPointerBug1", 3, 0);
          cmBug2 = cmConstructor.newInstance("ConcurrentModificationBug2", 1, 3);
          ntBug2 = ntConstructor.newInstance("NoneTerminationBug2", 1, 2);
          npBug2 = npConstructor.newInstance("NullPointerBug2", 1, 4);
          csStudent = csStudentConstructor.newInstance(2);
          addBugMethod.invoke(building, cmBug1); // 57 HP
          addBugMethod.invoke(building, ntBug1); // 200 HP
          addBugMethod.invoke(building, npBug1); // 52 HP
          addBugMethod.invoke(building, cmBug2); // 20 HP
          addBugMethod.invoke(building, ntBug2); // 200 HP
          addBugMethod.invoke(building, npBug2); // 10 HP
          bugsMoveMethod.invoke(building); // Bug enters the building
          bugsMoveMethod.invoke(building); // Bug enters the building
          bugsMoveMethod.invoke(building); // Bug enters the building
          bugsMoveMethod.invoke(building); // Bug enters the building
        } catch (InstantiationException | IllegalAccessException | IllegalArgumentException
            | InvocationTargetException e) {
          e.printStackTrace();
          fail(": Setting up failed");
          return;
        }
        try {
          Object result;
          Object[] bugs;

          // Attack 1: causes 14 damages to npBug1 => [npBug1(38), cmBug1(57), ntBug1(200), cmBug2(20), ntBug2(200)] [npBug2(10)]
          assertEquals(0, defenceMethod.invoke(csStudent, building),
              "There should be no knowledge points gained");
          assertEquals(38, getCurrentHpMethod.invoke(npBug1));
          assertEquals(57, getCurrentHpMethod.invoke(cmBug1));
          assertEquals(200, getCurrentHpMethod.invoke(ntBug1));
          assertEquals(10, getCurrentHpMethod.invoke(npBug2));
          assertEquals(20, getCurrentHpMethod.invoke(cmBug2));
          assertEquals(200, getCurrentHpMethod.invoke(ntBug2));
          result = getAllBugsMethod.invoke(building);
          if (!bugArrayClass.isInstance(result)) {
            fail("Expect to return an array of bugs");
          }
          bugs = (Object[]) result;
          assertEquals(5, bugs.length, ": There should be 5 bugs");
          assertEquals(npBug1, bugs[0], "The 1st bug should be NullPointerBug1");
          assertEquals(cmBug1, bugs[1], "The 2nd bug should be ConcurrentModificationBug1");
          assertEquals(ntBug1, bugs[2], "The 3rd bug should be NoneTerminationBug1");
          assertEquals(cmBug2, bugs[3], "The 4th bug should be ConcurrentModificationBug2");
          assertEquals(ntBug2, bugs[4], "The 5th bug should be NoneTerminationBug2");

          // Attack 2: causes 14 damages to npBug1 => [npBug1(24), cmBug1(57), ntBug1(200), cmBug2(20), ntBug2(200)] [npBug2(10)]
          assertEquals(0, defenceMethod.invoke(csStudent, building),
              "There should be no knowledge points gained");
          assertEquals(24, getCurrentHpMethod.invoke(npBug1));
          assertEquals(57, getCurrentHpMethod.invoke(cmBug1));
          assertEquals(200, getCurrentHpMethod.invoke(ntBug1));
          assertEquals(10, getCurrentHpMethod.invoke(npBug2));
          assertEquals(20, getCurrentHpMethod.invoke(cmBug2));
          assertEquals(200, getCurrentHpMethod.invoke(ntBug2));
          result = getAllBugsMethod.invoke(building);
          if (!bugArrayClass.isInstance(result)) {
            fail("Expect to return an array of bugs");
          }
          bugs = (Object[]) result;
          assertEquals(5, bugs.length, ": There should be 5 bugs");
          assertEquals(npBug1, bugs[0], "The 1st bug should be NullPointerBug1");
          assertEquals(cmBug1, bugs[1], "The 2nd bug should be ConcurrentModificationBug1");
          assertEquals(ntBug1, bugs[2], "The 3rd bug should be NoneTerminationBug1");
          assertEquals(cmBug2, bugs[3], "The 4th bug should be ConcurrentModificationBug2");
          assertEquals(ntBug2, bugs[4], "The 5th bug should be NoneTerminationBug2");

          // Attack 3: causes 14 damages to npBug1 => [npBug1(10), cmBug1(57), ntBug1(200), cmBug2(20), ntBug2(200)] [npBug2(10)]
          assertEquals(0, defenceMethod.invoke(csStudent, building),
              "There should be no knowledge points gained");
          assertEquals(10, getCurrentHpMethod.invoke(npBug1));
          assertEquals(57, getCurrentHpMethod.invoke(cmBug1));
          assertEquals(200, getCurrentHpMethod.invoke(ntBug1));
          assertEquals(10, getCurrentHpMethod.invoke(npBug2));
          assertEquals(20, getCurrentHpMethod.invoke(cmBug2));
          assertEquals(200, getCurrentHpMethod.invoke(ntBug2));
          result = getAllBugsMethod.invoke(building);
          if (!bugArrayClass.isInstance(result)) {
            fail("Expect to return an array of bugs");
          }
          bugs = (Object[]) result;
          assertEquals(5, bugs.length, ": There should be 5 bugs");
          assertEquals(npBug1, bugs[0], "The 1st bug should be NullPointerBug1");
          assertEquals(cmBug1, bugs[1], "The 2nd bug should be ConcurrentModificationBug1");
          assertEquals(ntBug1, bugs[2], "The 3rd bug should be NoneTerminationBug1");
          assertEquals(cmBug2, bugs[3], "The 4th bug should be ConcurrentModificationBug2");
          assertEquals(ntBug2, bugs[4], "The 5th bug should be NoneTerminationBug2");

          // Attack 4: causes 14 damages and remove npBug1 => [cmBug1(57), ntBug1(200), cmBug2(20), ntBug2(200)] [npBug2(10)]
          assertEquals(60, defenceMethod.invoke(csStudent, building),
              "There should be 60 knowledge points gained");
          assertEquals(0, getCurrentHpMethod.invoke(npBug1));
          assertEquals(57, getCurrentHpMethod.invoke(cmBug1));
          assertEquals(200, getCurrentHpMethod.invoke(ntBug1));
          assertEquals(10, getCurrentHpMethod.invoke(npBug2));
          assertEquals(20, getCurrentHpMethod.invoke(cmBug2));
          assertEquals(200, getCurrentHpMethod.invoke(ntBug2));
          result = getAllBugsMethod.invoke(building);
          if (!bugArrayClass.isInstance(result)) {
            fail("Expect to return an array of bugs");
          }
          bugs = (Object[]) result;
          assertEquals(4, bugs.length, ": There should be 4 bugs");
          assertEquals(cmBug1, bugs[0], "The 1st bug should be ConcurrentModificationBug1");
          assertEquals(ntBug1, bugs[1], "The 2nd bug should be NoneTerminationBug1");
          assertEquals(cmBug2, bugs[2], "The 3rd bug should be ConcurrentModificationBug2");
          assertEquals(ntBug2, bugs[3], "The 4th bug should be NoneTerminationBug2");

          // Attack 5: causes 14 damages to cmBug1 => [cmBug1(43), ntBug1(200), cmBug2(20), ntBug2(200)] [npBug2(10)]
          assertEquals(0, defenceMethod.invoke(csStudent, building),
              "There should be no knowledge points gained");
          assertEquals(0, getCurrentHpMethod.invoke(npBug1));
          assertEquals(43, getCurrentHpMethod.invoke(cmBug1));
          assertEquals(200, getCurrentHpMethod.invoke(ntBug1));
          assertEquals(10, getCurrentHpMethod.invoke(npBug2));
          assertEquals(20, getCurrentHpMethod.invoke(cmBug2));
          assertEquals(200, getCurrentHpMethod.invoke(ntBug2));
          result = getAllBugsMethod.invoke(building);
          if (!bugArrayClass.isInstance(result)) {
            fail("Expect to return an array of bugs");
          }
          bugs = (Object[]) result;
          assertEquals(4, bugs.length, ": There should be 4 bugs");
          assertEquals(cmBug1, bugs[0], "The 1st bug should be ConcurrentModificationBug1");
          assertEquals(ntBug1, bugs[1], "The 2nd bug should be NoneTerminationBug1");
          assertEquals(cmBug2, bugs[2], "The 3rd bug should be ConcurrentModificationBug2");
          assertEquals(ntBug2, bugs[3], "The 4th bug should be NoneTerminationBug2");

          // Attack 6 causes 56 damages and remove cmBug1 => [ntBug1(200), cmBug2(20), ntBug2(200)] [npBug2(10)]
          assertEquals(40, defenceMethod.invoke(csStudent, building),
              "There should be 40 knowledge points gained");
          assertEquals(0, getCurrentHpMethod.invoke(npBug1));
          assertEquals(0, getCurrentHpMethod.invoke(cmBug1));
          assertEquals(200, getCurrentHpMethod.invoke(ntBug1));
          assertEquals(10, getCurrentHpMethod.invoke(npBug2));
          assertEquals(20, getCurrentHpMethod.invoke(cmBug2));
          assertEquals(200, getCurrentHpMethod.invoke(ntBug2));
          result = getAllBugsMethod.invoke(building);
          if (!bugArrayClass.isInstance(result)) {
            fail("Expect to return an array of bugs");
          }
          bugs = (Object[]) result;
          assertEquals(3, bugs.length, ": There should be 3 bugs");
          assertEquals(ntBug1, bugs[0], "The third bug should be NoneTerminationBug1");
          assertEquals(cmBug2, bugs[1], "The second bug should be ConcurrentModificationBug1");
          assertEquals(ntBug2, bugs[2], "The first bug should be NoneTerminationBug2");
        } catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
          e.printStackTrace();
          fail(": Running test failed");
        }

      });
    }

  }

  /**
   * Test class for {@link students.CyberStudent}'s signature.
   *
   * @author htson
   * @version 1.0
   * @see AbstractTestCyberStudent
   */
  @Nested
  public class TestCyberStudentSignature extends AbstractTestCyberStudent {

    private Class<?> buildingClass;

    @BeforeEach
    public void setup() {
      super.setup();
      buildingClass = getClassOrInterface("building.Building");
    }

    @Test
    @DisplayName("Test students.CyberStudent's signature")
    public void testCyberStudent_Signature() {
      assertClass("");
      assertConstructor("", int.class);
      assertMethod("", "getLevel");
      assertMethod("", "upgradeCost");
      assertMethod("", "defence", buildingClass);
    }
  }

  /**
   * Test class for {@link students.CyberStudent}'s specification.
   *
   * @author htson
   * @version 1.0
   * @see TestCoursework.AbstractTestCyberStudent
   */
  @Nested
  public class TestCyberStudentSpecification extends AbstractTestCyberStudent {

    private Class<?> bugClass;
    private Class<?> buildingClass;
    private Class<?> cmClass;
    private Class<?> ntClass;
    private Class<?> npClass;
    private Class<?> bugArrayClass;

    @BeforeEach
    public void setup() {
      super.setup();
      bugClass = getClassOrInterface("bugs.Bug");
      buildingClass = getClassOrInterface("building.Building");
      cmClass = getClassOrInterface("bugs.ConcurrentModificationBug");
      cmClass = getClassOrInterface("bugs.ConcurrentModificationBug");
      ntClass = getClassOrInterface("bugs.NoneTerminationBug");
      npClass = getClassOrInterface("bugs.NullPointerBug");
      bugArrayClass = Array.newInstance(bugClass, 0).getClass();
    }

    @Test
    @DisplayName("Test students.CyberStudent's constructor and getters")
    public void testCyberStudent_constructor() {
      assertTimeout(ofMillis(1000), () -> {
        Constructor<?> cyberStudentConstructor = assertConstructor("", int.class);
        Method getLevelMethod = assertMethod("", "getLevel");
        Method getUpgradeCostMethod = assertMethod("", "upgradeCost");
        Object cyberStudent;
        try {
          cyberStudent = cyberStudentConstructor.newInstance(1);
        } catch (InstantiationException | IllegalAccessException | IllegalArgumentException
            | InvocationTargetException e) {
          e.printStackTrace();
          fail(": Setting up failed");
          return;
        }
        try {
          assertEquals(1, getLevelMethod.invoke(cyberStudent), ": Incorrect level for CyberStudent");
          assertEquals(200, getUpgradeCostMethod.invoke(cyberStudent),
              ": Incorrect upgrade cost for CyberStudent Level 1");
        } catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
          e.printStackTrace();
          fail(": Running test failed");
        }

      });
    }

    @Test
    @DisplayName("Test students.CyberStudent's defence method")
    public void testCyberStudent_defense() {
      assertTimeout(ofMillis(1000), () -> {
        Object cyberStudent;
        Object building;
        Object cmBug1;
        Object ntBug1;
        Object npBug1;
        Object cmBug2;
        Object ntBug2;
        Object npBug2;
        Method defenceMethod;
        Method getAllBugsMethod;
        Method getCurrentHpMethod;
        try {
          Constructor<?> cyberStudentConstructor = assertConstructor("", int.class);
          defenceMethod = assertAccessibleMethod("", "defence", buildingClass);
          Constructor<?> buildingConstructor = assertConstructor(buildingClass, "", int.class,
              int.class);
          Method addBugMethod = assertMethod(buildingClass, "", "addBug", bugClass);
          Method bugsMoveMethod = assertMethod(buildingClass, "", "bugsMove");
          getAllBugsMethod = assertMethod(buildingClass, "", "getAllBugs");
          getCurrentHpMethod = assertMethod(bugClass, "", "getCurrentHp");
          Constructor<?> cmConstructor = assertConstructor(cmClass, "", String.class, int.class,
              int.class);
          Constructor<?> ntConstructor = assertConstructor(ntClass, "", String.class, int.class,
              int.class);
          Constructor<?> npConstructor = assertConstructor(npClass, "", String.class, int.class,
              int.class);
          building = buildingConstructor.newInstance(4, 5);
          cmBug1 = cmConstructor.newInstance("ConcurrentModificationBug1", 2, 0);
          ntBug1 = ntConstructor.newInstance("NoneTerminationBug1", 1, 0);
          npBug1 = npConstructor.newInstance("NullPointerBug1", 3, 0);
          cmBug2 = cmConstructor.newInstance("ConcurrentModificationBug2", 1, 3);
          ntBug2 = ntConstructor.newInstance("NoneTerminationBug2", 1, 2);
          npBug2 = npConstructor.newInstance("NullPointerBug2", 1, 4);
          cyberStudent = cyberStudentConstructor.newInstance(3);
          addBugMethod.invoke(building, cmBug1); // 57 HP
          addBugMethod.invoke(building, ntBug1); // 200 HP
          addBugMethod.invoke(building, npBug1); // 52 HP
          addBugMethod.invoke(building, cmBug2); // 20 HP
          addBugMethod.invoke(building, ntBug2); // 200 HP
          addBugMethod.invoke(building, npBug2); // 10 HP
          bugsMoveMethod.invoke(building); // Bug enters the building
          bugsMoveMethod.invoke(building); // Bug enters the building
          bugsMoveMethod.invoke(building); // Bug enters the building
          bugsMoveMethod.invoke(building); // Bug enters the building
        } catch (InstantiationException | IllegalAccessException | IllegalArgumentException
            | InvocationTargetException e) {
          e.printStackTrace();
          fail(": Setting up failed");
          return;
        }
        try {
          Object result;
          Object[] bugs;

          // Attack 1: causes 26 damages to npBug1 => [npBug1(26), cmBug1(57), ntBug1(200), cmBug2(20), ntBug2(200)] [npBug2(10)]
          assertEquals(0, defenceMethod.invoke(cyberStudent, building),
              "There should be no knowledge points gained");
          assertEquals(26, getCurrentHpMethod.invoke(npBug1));
          assertEquals(57, getCurrentHpMethod.invoke(cmBug1));
          assertEquals(200, getCurrentHpMethod.invoke(ntBug1));
          assertEquals(10, getCurrentHpMethod.invoke(npBug2));
          assertEquals(20, getCurrentHpMethod.invoke(cmBug2));
          assertEquals(200, getCurrentHpMethod.invoke(ntBug2));
          result = getAllBugsMethod.invoke(building);
          if (!bugArrayClass.isInstance(result)) {
            fail("Expect to return an array of bugs");
          }
          bugs = (Object[]) result;
          assertEquals(5, bugs.length, ": There should be 5 bugs");
          assertEquals(npBug1, bugs[0], "The 1st bug should be NullPointerBug1");
          assertEquals(cmBug1, bugs[1], "The 2nd bug should be ConcurrentModificationBug1");
          assertEquals(ntBug1, bugs[2], "The 3rd bug should be NoneTerminationBug1");
          assertEquals(cmBug2, bugs[3], "The 4th bug should be ConcurrentModificationBug2");
          assertEquals(ntBug2, bugs[4], "The 5th bug should be NoneTerminationBug2");

          // Attack 2: causes 26 damages and remove npBug1 => [cmBug1(57), ntBug1(200), cmBug2(20), ntBug2(200)] [npBug2(10)]
          assertEquals(60, defenceMethod.invoke(cyberStudent, building),
              "There should be 60 knowledge points gained");
          assertEquals(0, getCurrentHpMethod.invoke(npBug1));
          assertEquals(57, getCurrentHpMethod.invoke(cmBug1));
          assertEquals(200, getCurrentHpMethod.invoke(ntBug1));
          assertEquals(10, getCurrentHpMethod.invoke(npBug2));
          assertEquals(20, getCurrentHpMethod.invoke(cmBug2));
          assertEquals(200, getCurrentHpMethod.invoke(ntBug2));
          result = getAllBugsMethod.invoke(building);
          if (!bugArrayClass.isInstance(result)) {
            fail("Expect to return an array of bugs");
          }
          bugs = (Object[]) result;
          assertEquals(4, bugs.length, ": There should be 4 bugs");
          assertEquals(cmBug1, bugs[0], "The 1st bug should be ConcurrentModificationBug1");
          assertEquals(ntBug1, bugs[1], "The 2nd bug should be NoneTerminationBug1");
          assertEquals(cmBug2, bugs[2], "The 3rd bug should be ConcurrentModificationBug2");
          assertEquals(ntBug2, bugs[3], "The 4th bug should be NoneTerminationBug2");

          // Attack 3: causes 26 damages to cmBug1 => [cmBug1(31), ntBug1(200), cmBug2(20), ntBug2(200)] [npBug2(10)]
          assertEquals(0, defenceMethod.invoke(cyberStudent, building),
              "There should be no knowledge points gained");
          assertEquals(0, getCurrentHpMethod.invoke(npBug1));
          assertEquals(31, getCurrentHpMethod.invoke(cmBug1));
          assertEquals(200, getCurrentHpMethod.invoke(ntBug1));
          assertEquals(10, getCurrentHpMethod.invoke(npBug2));
          assertEquals(20, getCurrentHpMethod.invoke(cmBug2));
          assertEquals(200, getCurrentHpMethod.invoke(ntBug2));
          result = getAllBugsMethod.invoke(building);
          if (!bugArrayClass.isInstance(result)) {
            fail("Expect to return an array of bugs");
          }
          bugs = (Object[]) result;
          assertEquals(4, bugs.length, ": There should be 4 bugs");
          assertEquals(cmBug1, bugs[0], "The 1st bug should be ConcurrentModificationBug1");
          assertEquals(ntBug1, bugs[1], "The 2nd bug should be NoneTerminationBug1");
          assertEquals(cmBug2, bugs[2], "The 3rd bug should be ConcurrentModificationBug2");
          assertEquals(ntBug2, bugs[3], "The 4th bug should be NoneTerminationBug2");

          // Attack 4: causes 26 damages to cmBug1 => [cmBug1(5), ntBug1(200), cmBug2(20), ntBug2(200)] [npBug2(10)]
          assertEquals(0, defenceMethod.invoke(cyberStudent, building),
              "There should be no knowledge points gained");
          assertEquals(0, getCurrentHpMethod.invoke(npBug1));
          assertEquals(5, getCurrentHpMethod.invoke(cmBug1));
          assertEquals(200, getCurrentHpMethod.invoke(ntBug1));
          assertEquals(10, getCurrentHpMethod.invoke(npBug2));
          assertEquals(20, getCurrentHpMethod.invoke(cmBug2));
          assertEquals(200, getCurrentHpMethod.invoke(ntBug2));
          result = getAllBugsMethod.invoke(building);
          if (!bugArrayClass.isInstance(result)) {
            fail("Expect to return an array of bugs");
          }
          bugs = (Object[]) result;
          assertEquals(4, bugs.length, ": There should be 4 bugs");
          assertEquals(cmBug1, bugs[0], "The 1st bug should be ConcurrentModificationBug1");
          assertEquals(ntBug1, bugs[1], "The 2nd bug should be NoneTerminationBug1");
          assertEquals(cmBug2, bugs[2], "The 3rd bug should be ConcurrentModificationBug2");
          assertEquals(ntBug2, bugs[3], "The 4th bug should be NoneTerminationBug2");

          // Attack 5: causes 26 damages and remove cmBug1 => [ntBug1(200), cmBug2(20), ntBug2(200)] [npBug2(10)]
          assertEquals(40, defenceMethod.invoke(cyberStudent, building),
              "There should be 40 knowledge points gained");
          assertEquals(0, getCurrentHpMethod.invoke(npBug1));
          assertEquals(0, getCurrentHpMethod.invoke(cmBug1));
          assertEquals(200, getCurrentHpMethod.invoke(ntBug1));
          assertEquals(10, getCurrentHpMethod.invoke(npBug2));
          assertEquals(20, getCurrentHpMethod.invoke(cmBug2));
          assertEquals(200, getCurrentHpMethod.invoke(ntBug2));
          result = getAllBugsMethod.invoke(building);
          if (!bugArrayClass.isInstance(result)) {
            fail("Expect to return an array of bugs");
          }
          bugs = (Object[]) result;
          assertEquals(3, bugs.length, ": There should be 3 bugs");
          assertEquals(ntBug1, bugs[0], "The 1st bug should be NoneTerminationBug1");
          assertEquals(cmBug2, bugs[1], "The 2nd bug should be ConcurrentModificationBug2");
          assertEquals(ntBug2, bugs[2], "The 3rd bug should be NoneTerminationBug2");

          // Attack 6 causes 26 damages to ntBug1 => [ntBug1(174), cmBug2(20), ntBug2(200)] [npBug2(10)]
          assertEquals(0, defenceMethod.invoke(cyberStudent, building),
              "There should be no knowledge points gained");
          assertEquals(0, getCurrentHpMethod.invoke(npBug1));
          assertEquals(0, getCurrentHpMethod.invoke(cmBug1));
          assertEquals(174, getCurrentHpMethod.invoke(ntBug1));
          assertEquals(10, getCurrentHpMethod.invoke(npBug2));
          assertEquals(20, getCurrentHpMethod.invoke(cmBug2));
          assertEquals(200, getCurrentHpMethod.invoke(ntBug2));
          result = getAllBugsMethod.invoke(building);
          if (!bugArrayClass.isInstance(result)) {
            fail("Expect to return an array of bugs");
          }
          bugs = (Object[]) result;
          assertEquals(3, bugs.length, ": There should be 3 bugs");
          assertEquals(ntBug1, bugs[0], "The 1st bug should be NoneTerminationBug1");
          assertEquals(cmBug2, bugs[1], "The 2nd bug should be ConcurrentModificationBug2");
          assertEquals(ntBug2, bugs[2], "The 3rd bug should be NoneTerminationBug2");

          // Attack 7 causes 26 damages to ntBug1 => [ntBug1(148), cmBug2(20), ntBug2(200)] [npBug2(10)]
          assertEquals(0, defenceMethod.invoke(cyberStudent, building),
              "There should be no knowledge points gained");
          assertEquals(0, getCurrentHpMethod.invoke(npBug1));
          assertEquals(0, getCurrentHpMethod.invoke(cmBug1));
          assertEquals(148, getCurrentHpMethod.invoke(ntBug1));
          assertEquals(10, getCurrentHpMethod.invoke(npBug2));
          assertEquals(20, getCurrentHpMethod.invoke(cmBug2));
          assertEquals(200, getCurrentHpMethod.invoke(ntBug2));
          result = getAllBugsMethod.invoke(building);
          if (!bugArrayClass.isInstance(result)) {
            fail("Expect to return an array of bugs");
          }
          bugs = (Object[]) result;
          assertEquals(3, bugs.length, ": There should be 3 bugs");
          assertEquals(ntBug1, bugs[0], "The 1st bug should be NoneTerminationBug1");
          assertEquals(cmBug2, bugs[1], "The 2nd bug should be ConcurrentModificationBug2");
          assertEquals(ntBug2, bugs[2], "The 3rd bug should be NoneTerminationBug2");

          // Attack 8 causes either 52 damages or remove ntBug1 => [ntBug1(148), cmBug2(20), ntBug2(200)] [npBug2(10)]
          Object knowledgePoints = defenceMethod.invoke(cyberStudent, building);
          System.out.println(knowledgePoints);
          System.out.println(getCurrentHpMethod.invoke(ntBug1));
          System.out.println(Integer.valueOf(96));
          System.out.println(Integer.valueOf(96) == getCurrentHpMethod.invoke(ntBug1));
          if (Integer.valueOf(96) == getCurrentHpMethod.invoke(ntBug1)) { // Double damage
            assertEquals(0, knowledgePoints,
                "There should be no knowledge points gained");
          } else {
            assertEquals(20, knowledgePoints,
                "There should be 20 knowledge points gained");
          }
          assertEquals(0, getCurrentHpMethod.invoke(npBug1));
          assertEquals(0, getCurrentHpMethod.invoke(cmBug1));
          assertEquals(10, getCurrentHpMethod.invoke(npBug2));
          assertEquals(20, getCurrentHpMethod.invoke(cmBug2));
          assertEquals(200, getCurrentHpMethod.invoke(ntBug2));
          result = getAllBugsMethod.invoke(building);
          if (!bugArrayClass.isInstance(result)) {
            fail("Expect to return an array of bugs");
          }
          bugs = (Object[]) result;

          if (Integer.valueOf(96).equals(getCurrentHpMethod.invoke(ntBug1))) { // Double damage
            assertEquals(3, bugs.length, ": There should be 3 bugs");
            assertEquals(ntBug1, bugs[0], "The 1st bug should be NoneTerminationBug1");
            assertEquals(cmBug2, bugs[1], "The 2nd bug should be ConcurrentModificationBug2");
            assertEquals(ntBug2, bugs[2], "The 3rd bug should be NoneTerminationBug2");
          } else { // Instant removal
            assertEquals(2, bugs.length, ": There should be 2 bugs");
            assertEquals(cmBug2, bugs[0], "The 1st bug should be ConcurrentModificationBug2");
            assertEquals(ntBug2, bugs[1], "The 2nd bug should be NoneTerminationBug2");
          }
        } catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
          e.printStackTrace();
          fail(": Running test failed");
        }

      });
    }

  }

  /**
   * Test class for {@link students.SeStudent}'s signature.
   *
   * @author htson
   * @version 1.0
   * @see AbstractTestSeStudent
   */
  @Nested
  public class TestSeStudentSignature extends AbstractTestSeStudent {

    private Class<?> buildingClass;

    @BeforeEach
    public void setup() {
      super.setup();
      buildingClass = getClassOrInterface("building.Building");
    }

    @Test
    @DisplayName("Test students.SeStudent's signature")
    public void testSeStudent_Signature() {
      assertClass("");
      assertConstructor("", int.class);
      assertMethod("", "getLevel");
      assertMethod("", "upgradeCost");
      assertMethod("", "defence", buildingClass);
    }
  }

  /**
   * Test class for {@link students.SeStudent}'s specification.
   *
   * @author htson
   * @version 1.0
   * @see TestCoursework.AbstractTestSeStudent
   */
  @Nested
  public class TestSeStudentSpecification extends AbstractTestSeStudent {

    private Class<?> bugClass;
    private Class<?> buildingClass;
    private Class<?> cmClass;
    private Class<?> ntClass;
    private Class<?> npClass;
    private Class<?> bugArrayClass;

    @BeforeEach
    public void setup() {
      super.setup();
      bugClass = getClassOrInterface("bugs.Bug");
      buildingClass = getClassOrInterface("building.Building");
      cmClass = getClassOrInterface("bugs.ConcurrentModificationBug");
      cmClass = getClassOrInterface("bugs.ConcurrentModificationBug");
      ntClass = getClassOrInterface("bugs.NoneTerminationBug");
      npClass = getClassOrInterface("bugs.NullPointerBug");
      bugArrayClass = Array.newInstance(bugClass, 0).getClass();
    }

    @Test
    @DisplayName("Test students.SeStudent's constructor and getters")
    public void testSeStudent_constructor() {
      assertTimeout(ofMillis(1000), () -> {
        Constructor<?> seStudentConstructor = assertConstructor("", int.class);
        Method getLevelMethod = assertMethod("", "getLevel");
        Method getUpgradeCostMethod = assertMethod("", "upgradeCost");
        Object seStudent;
        try {
          seStudent = seStudentConstructor.newInstance(1);
        } catch (InstantiationException | IllegalAccessException | IllegalArgumentException
            | InvocationTargetException e) {
          e.printStackTrace();
          fail(": Setting up failed");
          return;
        }
        try {
          assertEquals(1, getLevelMethod.invoke(seStudent), ": Incorrect level for SeStudent");
          assertEquals(200, getUpgradeCostMethod.invoke(seStudent),
              ": Incorrect upgrade cost for SeStudent Level 1");
        } catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
          e.printStackTrace();
          fail(": Running test failed");
        }

      });
    }

    @Test
    @DisplayName("Test students.SeStudent's defence method")
    public void testSeStudent_defense() {
      assertTimeout(ofMillis(1000), () -> {
        Object seStudent;
        Object building;
        Object cmBug1;
        Object ntBug1;
        Object npBug1;
        Object cmBug2;
        Object ntBug2;
        Object npBug2;
        Method defenceMethod;
        Method getAllBugsMethod;
        Method getCurrentHpMethod;
        Method getCurrentStepsMethod;
        Method getCurrentFloorMethod;
        try {
          Constructor<?> seStudentConstructor = assertConstructor("", int.class);
          defenceMethod = assertAccessibleMethod("", "defence", buildingClass);
          Constructor<?> buildingConstructor = assertConstructor(buildingClass, "", int.class,
              int.class);
          Method addBugMethod = assertMethod(buildingClass, "", "addBug", bugClass);
          Method bugsMoveMethod = assertMethod(buildingClass, "", "bugsMove");
          getAllBugsMethod = assertMethod(buildingClass, "", "getAllBugs");
          getCurrentHpMethod = assertMethod(bugClass, "", "getCurrentHp");
          getCurrentStepsMethod = assertMethod(bugClass, "", "getCurrentSteps");
          getCurrentFloorMethod = assertMethod(bugClass, "", "getCurrentFloor");
          Constructor<?> cmConstructor = assertConstructor(cmClass, "", String.class, int.class,
              int.class);
          Constructor<?> ntConstructor = assertConstructor(ntClass, "", String.class, int.class,
              int.class);
          Constructor<?> npConstructor = assertConstructor(npClass, "", String.class, int.class,
              int.class);
          building = buildingConstructor.newInstance(4, 5);
          cmBug1 = cmConstructor.newInstance("ConcurrentModificationBug1", 2, 0);
          ntBug1 = ntConstructor.newInstance("NoneTerminationBug1", 1, 0);
          npBug1 = npConstructor.newInstance("NullPointerBug1", 3, 0);
          cmBug2 = cmConstructor.newInstance("ConcurrentModificationBug2", 1, 3);
          ntBug2 = ntConstructor.newInstance("NoneTerminationBug2", 1, 2);
          npBug2 = npConstructor.newInstance("NullPointerBug2", 1, 4);
          seStudent = seStudentConstructor.newInstance(1);
          addBugMethod.invoke(building, cmBug1); // 57 HP
          addBugMethod.invoke(building, ntBug1); // 200 HP
          addBugMethod.invoke(building, npBug1); // 52 HP
          addBugMethod.invoke(building, cmBug2); // 20 HP
          addBugMethod.invoke(building, ntBug2); // 200 HP
          addBugMethod.invoke(building, npBug2); // 10 HP
          bugsMoveMethod.invoke(building); // Bug enters the building
          bugsMoveMethod.invoke(building); // Bug enters the building
          bugsMoveMethod.invoke(building); // Bug enters the building
          bugsMoveMethod.invoke(building); // Bug enters the building
          // cmBug1[0, -1] [3, 0] [2, 0] [1, 0] [0, 0]
          // ntBug1[0, -1] [5, 0] [4, 0] [3, 0] [2, 0]
          // npBug1[0, -1] [1, 0] [0, 0] [1, 0] [0, 0]
          // cmBug2[3, -1] [2, -1] [1, -1] [0, -1] [0, 0]
          // ntBug2[2, -1] [1, -1] [0, -1] [5, 0] [4, 0]
          // npBug2[4, -1] [3, -1] [2, -1] [1, -1] [0, -1]
        } catch (InstantiationException | IllegalAccessException | IllegalArgumentException
            | InvocationTargetException e) {
          e.printStackTrace();
          fail(": Setting up failed");
          return;
        }
        try {
          Object result;
          Object[] bugs;

          // Attack 1: causes 5 damages to npBug1 => [npBug1(47), cmBug1(57), ntBug1(200), cmBug2(20), ntBug2(200)] [npBug2(10)]
          assertEquals(0, defenceMethod.invoke(seStudent, building),
              "There should be no knowledge points gained");
          assertEquals(47, getCurrentHpMethod.invoke(npBug1));
          assertEquals(57, getCurrentHpMethod.invoke(cmBug1));
          assertEquals(200, getCurrentHpMethod.invoke(ntBug1));
          assertEquals(10, getCurrentHpMethod.invoke(npBug2));
          assertEquals(20, getCurrentHpMethod.invoke(cmBug2));
          assertEquals(200, getCurrentHpMethod.invoke(ntBug2));
          result = getAllBugsMethod.invoke(building);
          if (!bugArrayClass.isInstance(result)) {
            fail("Expect to return an array of bugs");
          }
          bugs = (Object[]) result;
          assertEquals(5, bugs.length, ": There should be 5 bugs");
          assertEquals(npBug1, bugs[0], "The 1st bug should be NullPointerBug1");
          assertEquals(cmBug1, bugs[1], "The 2nd bug should be ConcurrentModificationBug1");
          assertEquals(ntBug1, bugs[2], "The 3rd bug should be NoneTerminationBug1");
          assertEquals(cmBug2, bugs[3], "The 4th bug should be ConcurrentModificationBug2");
          assertEquals(ntBug2, bugs[4], "The 5th bug should be NoneTerminationBug2");

          // Attack 2: causes 5 damages to npBug1 => [npBug1(42), cmBug1(57), ntBug1(200), cmBug2(20), ntBug2(200)] [npBug2(10)]
          assertEquals(0, defenceMethod.invoke(seStudent, building),
              "There should be no knowledge points gained");
          assertEquals(42, getCurrentHpMethod.invoke(npBug1));
          assertEquals(57, getCurrentHpMethod.invoke(cmBug1));
          assertEquals(200, getCurrentHpMethod.invoke(ntBug1));
          assertEquals(10, getCurrentHpMethod.invoke(npBug2));
          assertEquals(20, getCurrentHpMethod.invoke(cmBug2));
          assertEquals(200, getCurrentHpMethod.invoke(ntBug2));
          result = getAllBugsMethod.invoke(building);
          if (!bugArrayClass.isInstance(result)) {
            fail("Expect to return an array of bugs");
          }
          bugs = (Object[]) result;
          assertEquals(5, bugs.length, ": There should be 5 bugs");
          assertEquals(npBug1, bugs[0], "The 1st bug should be NullPointerBug1");
          assertEquals(cmBug1, bugs[1], "The 2nd bug should be ConcurrentModificationBug1");
          assertEquals(ntBug1, bugs[2], "The 3rd bug should be NoneTerminationBug1");
          assertEquals(cmBug2, bugs[3], "The 4th bug should be ConcurrentModificationBug2");
          assertEquals(ntBug2, bugs[4], "The 5th bug should be NoneTerminationBug2");

          // Attack 3: causes 5 damages to npBug1 => [npBug1(37), cmBug1(57), ntBug1(200), cmBug2(20), ntBug2(200)] [npBug2(10)]
          assertEquals(0, defenceMethod.invoke(seStudent, building),
              "There should be no knowledge points gained");
          assertEquals(37, getCurrentHpMethod.invoke(npBug1));
          assertEquals(57, getCurrentHpMethod.invoke(cmBug1));
          assertEquals(200, getCurrentHpMethod.invoke(ntBug1));
          assertEquals(10, getCurrentHpMethod.invoke(npBug2));
          assertEquals(20, getCurrentHpMethod.invoke(cmBug2));
          assertEquals(200, getCurrentHpMethod.invoke(ntBug2));
          result = getAllBugsMethod.invoke(building);
          if (!bugArrayClass.isInstance(result)) {
            fail("Expect to return an array of bugs");
          }
          bugs = (Object[]) result;
          assertEquals(5, bugs.length, ": There should be 5 bugs");
          assertEquals(npBug1, bugs[0], "The 1st bug should be NullPointerBug1");
          assertEquals(cmBug1, bugs[1], "The 2nd bug should be ConcurrentModificationBug1");
          assertEquals(ntBug1, bugs[2], "The 3rd bug should be NoneTerminationBug1");
          assertEquals(cmBug2, bugs[3], "The 4th bug should be ConcurrentModificationBug2");
          assertEquals(ntBug2, bugs[4], "The 5th bug should be NoneTerminationBug2");

          // Attack 4: causes 5 damages to npBug1 => [npBug1(32), cmBug1(57), ntBug1(200), cmBug2(20), ntBug2(200)] [npBug2(10)]
          assertEquals(0, defenceMethod.invoke(seStudent, building),
              "There should be no knowledge points gained");
          assertEquals(32, getCurrentHpMethod.invoke(npBug1));
          assertEquals(57, getCurrentHpMethod.invoke(cmBug1));
          assertEquals(200, getCurrentHpMethod.invoke(ntBug1));
          assertEquals(10, getCurrentHpMethod.invoke(npBug2));
          assertEquals(20, getCurrentHpMethod.invoke(cmBug2));
          assertEquals(200, getCurrentHpMethod.invoke(ntBug2));
          result = getAllBugsMethod.invoke(building);
          if (!bugArrayClass.isInstance(result)) {
            fail("Expect to return an array of bugs");
          }
          bugs = (Object[]) result;
          assertEquals(5, bugs.length, ": There should be 5 bugs");
          assertEquals(npBug1, bugs[0], "The 1st bug should be NullPointerBug1");
          assertEquals(cmBug1, bugs[1], "The 2nd bug should be ConcurrentModificationBug1");
          assertEquals(ntBug1, bugs[2], "The 3rd bug should be NoneTerminationBug1");
          assertEquals(cmBug2, bugs[3], "The 4th bug should be ConcurrentModificationBug2");
          assertEquals(ntBug2, bugs[4], "The 5th bug should be NoneTerminationBug2");

          // Attack 5: causes 5 damages to npBug1 => [npBug1(27), cmBug1(57), ntBug1(200), cmBug2(20), ntBug2(200)] [npBug2(10)]
          assertEquals(0, defenceMethod.invoke(seStudent, building),
              "There should be no knowledge points gained");
          assertEquals(27, getCurrentHpMethod.invoke(npBug1));
          assertEquals(57, getCurrentHpMethod.invoke(cmBug1));
          assertEquals(200, getCurrentHpMethod.invoke(ntBug1));
          assertEquals(10, getCurrentHpMethod.invoke(npBug2));
          assertEquals(20, getCurrentHpMethod.invoke(cmBug2));
          assertEquals(200, getCurrentHpMethod.invoke(ntBug2));
          result = getAllBugsMethod.invoke(building);
          if (!bugArrayClass.isInstance(result)) {
            fail("Expect to return an array of bugs");
          }
          bugs = (Object[]) result;
          assertEquals(5, bugs.length, ": There should be 5 bugs");
          assertEquals(npBug1, bugs[0], "The 1st bug should be NullPointerBug1");
          assertEquals(cmBug1, bugs[1], "The 2nd bug should be ConcurrentModificationBug1");
          assertEquals(ntBug1, bugs[2], "The 3rd bug should be NoneTerminationBug1");
          assertEquals(cmBug2, bugs[3], "The 4th bug should be ConcurrentModificationBug2");
          assertEquals(ntBug2, bugs[4], "The 5th bug should be NoneTerminationBug2");

          // Attack 6: causes 0 damages and slow down all bugs => [npBug1(27), cmBug1(57), ntBug1(200), cmBug2(20), ntBug2(200)] [npBug2(10)]
          assertEquals(0, defenceMethod.invoke(seStudent, building),
              "There should be no knowledge points gained");
          assertEquals(27, getCurrentHpMethod.invoke(npBug1));
          assertEquals(57, getCurrentHpMethod.invoke(cmBug1));
          assertEquals(200, getCurrentHpMethod.invoke(ntBug1));
          assertEquals(10, getCurrentHpMethod.invoke(npBug2));
          assertEquals(20, getCurrentHpMethod.invoke(cmBug2));
          assertEquals(200, getCurrentHpMethod.invoke(ntBug2));
          // cmBug1[0, -1] [3, 0] [2, 0] [1, 0] [2, 0]
          // ntBug1[0, -1] [5, 0] [4, 0] [3, 0] [4, 0]
          // npBug1[0, -1] [1, 0] [0, 0] [1, 1] [2, 1]
          // cmBug2[3, -1] [2, -1] [1, -1] [0, -1] [5, 0]
          // ntBug2[2, -1] [1, -1] [0, -1] [5, 0] [6, 0]
          // npBug2[4, -1] [3, -1] [2, -1] [1, -1] [0, -1]
          assertEquals(2, getCurrentStepsMethod.invoke(npBug1));
          assertEquals(2, getCurrentStepsMethod.invoke(cmBug1));
          assertEquals(4, getCurrentStepsMethod.invoke(ntBug1));
          assertEquals(0, getCurrentStepsMethod.invoke(npBug2));
          assertEquals(5, getCurrentStepsMethod.invoke(cmBug2));
          assertEquals(6, getCurrentStepsMethod.invoke(ntBug2));
          assertEquals(1, getCurrentFloorMethod.invoke(npBug1));
          assertEquals(0, getCurrentFloorMethod.invoke(cmBug1));
          assertEquals(0, getCurrentFloorMethod.invoke(ntBug1));
          assertEquals(-1, getCurrentFloorMethod.invoke(npBug2));
          assertEquals(0, getCurrentFloorMethod.invoke(cmBug2));
          assertEquals(0, getCurrentFloorMethod.invoke(ntBug2));
          result = getAllBugsMethod.invoke(building);
          if (!bugArrayClass.isInstance(result)) {
            fail("Expect to return an array of bugs");
          }
          bugs = (Object[]) result;
          assertEquals(5, bugs.length, ": There should be 5 bugs");
          assertEquals(npBug1, bugs[0], "The 1st bug should be NullPointerBug1");
          assertEquals(cmBug1, bugs[1], "The 2nd bug should be ConcurrentModificationBug1");
          assertEquals(ntBug1, bugs[2], "The 3rd bug should be NoneTerminationBug1");
          assertEquals(cmBug2, bugs[3], "The 4th bug should be ConcurrentModificationBug2");
          assertEquals(ntBug2, bugs[4], "The 5th bug should be NoneTerminationBug2");

        } catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
          e.printStackTrace();
          fail(": Running test failed");
        }

      });
    }

  }

}

