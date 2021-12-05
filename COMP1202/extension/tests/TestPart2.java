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
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.lang.reflect.Array;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

public class TestPart2 extends TestCoursework {

  /**
   * Test class for {@link building.Building}'s signature.
   *
   * @author htson
   * @version 1.0
   * @see TestCoursework.AbstractTestBuilding
   */
  @Nested
  public class TestBuildingSignature extends AbstractTestBuilding {

    private Class<?> bugClass;
    private Class<?> bugArrayClass;

    @BeforeEach
    public void setup() {
      super.setup();
      bugClass = getClassOrInterface("bugs.Bug");
      assertClass("", bugClass);
      bugArrayClass = Array.newInstance(bugClass, 0).getClass();
    }

    @Test
    @DisplayName("Test building.Building's signature")
    public void testBuilding_Signature() {
      assertClass("");
      assertDeclaredField("", "constructionPoints");
      assertDeclaredField("", "topFloor");
      assertDeclaredField("", "bugs");
      assertConstructor("", int.class, int.class);
      assertMethod("", "getTopFloor");
      assertMethod("", "getConstructionPoints");
      assertMethod("", bugArrayClass, "getAllBugs");
      assertMethod("", int.class, "addBug", bugClass);
      assertMethod("", "removeBug", bugClass);
      assertMethod("", "bugsMove");
    }
  }

  /**
   * Test class for {@link building.Building}'s specification.
   *
   * @author htson
   * @version 1.0
   * @see TestCoursework.AbstractTestBuilding
   */
  @Nested
  public class TestBuildingSpecification extends AbstractTestBuilding {

    private Class<?> bugClass;
    private Class<?> cmClass;
    private Class<?> ntClass;
    private Class<?> npClass;
    private Class<?> bugArrayClass;

    @BeforeEach
    public void setup() {
      super.setup();
      bugClass = getClassOrInterface("bugs.Bug");
      assertClass("", bugClass);
      cmClass = getClassOrInterface("bugs.ConcurrentModificationBug");
      assertClass("", cmClass);
      ntClass = getClassOrInterface("bugs.NoneTerminationBug");
      assertClass("", ntClass);
      npClass = getClassOrInterface("bugs.NullPointerBug");
      assertClass("", npClass);
      bugArrayClass = Array.newInstance(bugClass, 0).getClass();
    }

    @Test
    @DisplayName("Test building.Building's constructor and getters")
    public void testBuilding_constructor() {
      assertTimeout(ofMillis(1000), () -> {
        Constructor<?> buildingConstructor = assertConstructor("", int.class, int.class);
        Method getTopFloorMethod = assertMethod("", "getTopFloor");
        Method getConstructionPointsMethod = assertMethod("", "getConstructionPoints");
        Object building;
        try {
          building = buildingConstructor.newInstance(1, 5);
        } catch (InstantiationException | IllegalAccessException | IllegalArgumentException
            | InvocationTargetException e) {
          e.printStackTrace();
          fail(": Setting up failed");
          return;
        }
        try {
          assertEquals(1, getTopFloorMethod.invoke(building), ": Incorrect top floor");
          assertEquals(5, getConstructionPointsMethod.invoke(building),
              ": Incorrect construction points");
        } catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
          e.printStackTrace();
          fail(": Running test failed");
        }

      });
    }

    @Test
    @DisplayName("Test building.Building's getAllBugs")
    public void testBuilding_getAllBugs() {
      assertTimeout(ofMillis(1000), () -> {
        Constructor<?> buildingConstructor = assertConstructor("", int.class, int.class);
        Method getAllBugsMethod = assertMethod("", "getAllBugs");
        Method addBugMethod = assertMethod("", "addBug", bugClass);
        Method bugsMoveMethod = assertMethod("", "bugsMove");
        Constructor<?> cmConstructor = assertConstructor(cmClass, "", String.class, int.class,
            int.class);
        Constructor<?> ntConstructor = assertConstructor(ntClass, "", String.class, int.class,
            int.class);
        Constructor<?> npConstructor = assertConstructor(npClass, "", String.class, int.class,
            int.class);
        Object building;
        Object cmBug;
        Object ntBug;
        Object npBug;
        try {
          building = buildingConstructor.newInstance(1, 5);
          cmBug = cmConstructor.newInstance("ConcurrentModificationBug", 1, 1);
          ntBug = ntConstructor.newInstance("NoneTerminationBug", 2, 0);
          npBug = npConstructor.newInstance("NullPointerBug", 3, 2);
        } catch (InstantiationException | IllegalAccessException | IllegalArgumentException
            | InvocationTargetException e) {
          e.printStackTrace();
          fail(": Setting up failed");
          return;
        }

        try {
          Object[] bugs;
          Object result;

          // Initially empty
          result = getAllBugsMethod.invoke(building);
          if (!bugArrayClass.isInstance(result)) {
            fail("Expect to return an array of bugs");
          }
          bugs = (Object[]) result;
          assertEquals(0, bugs.length, ": There should be no bugs");

          // Add the bugs, but the bugs are still underground [] [cmBug(1, -1), ntBug(0, -1), np(2, -1)]
          addBugMethod.invoke(building, cmBug);
          addBugMethod.invoke(building, ntBug);
          addBugMethod.invoke(building, npBug);
          result = getAllBugsMethod.invoke(building);
//          if (!bugArrayClass.isInstance(result)) {
//            fail("Expect to return an array of bugs");
//          }
          bugs = (Object[]) result;
          assertEquals(0, bugs.length, ": There should be no bugs");

          // Move 1: Now there should be 1 bug [ntBug(5, 0)] [cmBug(0, -1), np(1, -1)]
          bugsMoveMethod.invoke(building);
          result = getAllBugsMethod.invoke(building);
//          if (!bugArrayClass.isInstance(result)) {
//            fail("Expect to return an array of bugs");
//          }
          bugs = (Object[]) result;
          assertEquals(1, bugs.length, ": There should be no bugs");
          assertEquals(ntBug, bugs[0], "The first bug should be NoneTerminationBug");

          // Move 2: Now there should be 2 bugs [cmBug(3, 0), ntBug(4, 0)] [np(0, -1)]
          bugsMoveMethod.invoke(building);
          result = getAllBugsMethod.invoke(building);
//          if (!bugArrayClass.isInstance(result)) {
//            fail("Expect to return an array of bugs");
//          }
          bugs = (Object[]) result;
          assertEquals(2, bugs.length, ": There should be no bugs");
          assertEquals(cmBug, bugs[0], "The first bug should be ConcurrentModificationBug");
          assertEquals(ntBug, bugs[1], "The first bug should be NoneTerminationBug");

          // Move 3: Now there should be 3 bugs [np(1, 0), cmBug(2, 0), ntBug(4, 0)] []
          bugsMoveMethod.invoke(building);
          result = getAllBugsMethod.invoke(building);
//          if (!bugArrayClass.isInstance(result)) {
//            fail("Expect to return an array of bugs");
//          }
          bugs = (Object[]) result;
          assertEquals(3, bugs.length, ": There should be no bugs");
          assertEquals(npBug, bugs[0], "The first bug should be NullPointerBug");
          assertEquals(cmBug, bugs[1], "The first bug should be ConcurrentModificationBug");
          assertEquals(ntBug, bugs[2], "The first bug should be NoneTerminationBug");
        } catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
          e.printStackTrace();
          fail(": Running test failed");
        }

      });
    }

    @Test
    @DisplayName("Test building.Building's addBug")
    public void testBuilding_addBug() {
      assertTimeout(ofMillis(1000), () -> {
        Constructor<?> buildingConstructor = assertConstructor("", int.class, int.class);
        Method addBugMethod = assertMethod("", int.class, "addBug", bugClass);
        Constructor<?> cmConstructor = assertConstructor(cmClass, "", String.class, int.class,
            int.class);
        Constructor<?> ntConstructor = assertConstructor(ntClass, "", String.class, int.class,
            int.class);
        Constructor<?> npConstructor = assertConstructor(npClass, "", String.class, int.class,
            int.class);
        Object building;
        Object cmBug;
        Object ntBug;
        Object npBug;
        try {
          building = buildingConstructor.newInstance(1, 5);
          cmBug = cmConstructor.newInstance("ConcurrentModificationBug", 1, 3);
          ntBug = ntConstructor.newInstance("NoneTerminationBug", 2, 5);
          npBug = npConstructor.newInstance("NullPointerBug", 3, 2);
        } catch (InstantiationException | IllegalAccessException | IllegalArgumentException
            | InvocationTargetException e) {
          e.printStackTrace();
          fail(": Setting up failed");
          return;
        }

        try {
          Object[] bugs;
          int result;
          assertEquals(1, addBugMethod.invoke(building, cmBug),
              "Expected to have 1 bug in the building");
          assertEquals(-1, addBugMethod.invoke(building, cmBug),
              "Expected to fail to add the ConcurrentModificationBug to the building");
          assertEquals(2, addBugMethod.invoke(building, ntBug),
              "Expected to have 2 bugs in the building");
          assertEquals(-1, addBugMethod.invoke(building, ntBug),
              "Expected to fail to add the NoneTerminationBug to the building");
          assertEquals(3, addBugMethod.invoke(building, npBug),
              "Expected to have 3 bugs in the building");
          assertEquals(-1, addBugMethod.invoke(building, npBug),
              "Expected to fail to add the NullPointerBug to the building");

        } catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
          e.printStackTrace();
          fail(": Running test failed");
        }

      });
    }

    @Test
    @DisplayName("Test building.Building's removeBug")
    public void testBuilding_removeBug() {
      assertTimeout(ofMillis(1000), () -> {
        Constructor<?> buildingConstructor = assertConstructor("", int.class, int.class);
        Method getAllBugsMethod = assertMethod("", "getAllBugs");
        Method bugsMoveMethod = assertMethod("", "bugsMove");
        Method addBugMethod = assertMethod("", "addBug", bugClass);
        Method removeBugMethod = assertMethod("", "removeBug", bugClass);
        Constructor<?> cmConstructor = assertConstructor(cmClass, "", String.class, int.class,
            int.class);
        Constructor<?> ntConstructor = assertConstructor(ntClass, "", String.class, int.class,
            int.class);
        Constructor<?> npConstructor = assertConstructor(npClass, "", String.class, int.class,
            int.class);
        Object building;
        Object cmBug;
        Object ntBug;
        Object npBug;
        try {
          building = buildingConstructor.newInstance(1, 5);
          cmBug = cmConstructor.newInstance("ConcurrentModificationBug", 1, 0);
          ntBug = ntConstructor.newInstance("NoneTerminationBug", 2, 0);
          npBug = npConstructor.newInstance("NullPointerBug", 3, 0);
        } catch (InstantiationException | IllegalAccessException | IllegalArgumentException
            | InvocationTargetException e) {
          e.printStackTrace();
          fail(": Setting up failed");
          return;
        }

        try {
          Object[] bugs;
          Object result;

          // Add the bugs
          addBugMethod.invoke(building, cmBug);
          addBugMethod.invoke(building, ntBug);
          addBugMethod.invoke(building, npBug);
          bugsMoveMethod.invoke(building);
          // Now there should be 3 bugs sorted [npBug, cmBug, ntBug]
          result = getAllBugsMethod.invoke(building);
          if (!bugArrayClass.isInstance(result)) {
            fail("Expect to return an array of bugs");
          }
          bugs = (Object[]) result;
          assertEquals(3, bugs.length, ": There should be 3 bugs");
          assertEquals(npBug, bugs[0], "The first bug should be NullPointerBug");
          assertEquals(cmBug, bugs[1], "The second bug should be ConcurrentModificationBug");
          assertEquals(ntBug, bugs[2], "The third bug should be NoneTerminationBug");

          // Remove cmBug
          removeBugMethod.invoke(building, cmBug);
          result = getAllBugsMethod.invoke(building);
          if (!bugArrayClass.isInstance(result)) {
            fail("Expect to return an array of bugs");
          }
          bugs = (Object[]) result;
          assertEquals(2, bugs.length, ": There should be 2 bugs");
          assertEquals(npBug, bugs[0], "The first bug should be NullPointerBug");
          assertEquals(ntBug, bugs[1], "The second bug should be NoneTerminationBug");

          // Remove ntBug
          removeBugMethod.invoke(building, ntBug);
          result = getAllBugsMethod.invoke(building);
          if (!bugArrayClass.isInstance(result)) {
            fail("Expect to return an array of bugs");
          }
          bugs = (Object[]) result;
          assertEquals(1, bugs.length, ": There should be 1 bug");
          assertEquals(npBug, bugs[0], "The first bug should be NullPointerBug");

          // Remove npBug
          removeBugMethod.invoke(building, npBug);
          result = getAllBugsMethod.invoke(building);
          if (!bugArrayClass.isInstance(result)) {
            fail("Expect to return an array of bugs");
          }
          bugs = (Object[]) result;
          assertEquals(0, bugs.length, ": There should be no bugs");

        } catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
          e.printStackTrace();
          fail(": Running test failed");
        }

      });
    }

    @Test
    @DisplayName("Test building.Building's bugsMove")
    public void testBuilding_bugsMove() {
      assertTimeout(ofMillis(1000), () -> {
        Constructor<?> buildingConstructor = assertConstructor("", int.class, int.class);
        Method bugsMoveMethod = assertMethod("", "bugsMove");
        Method getAllBugsMethod = assertMethod("", "getAllBugs");
        Method addBugMethod = assertMethod("", "addBug", bugClass);
        Method getConstructionPointsMethod = assertMethod("", "getConstructionPoints");
        Constructor<?> cmConstructor = assertConstructor(cmClass, "", String.class, int.class,
            int.class);
        Constructor<?> ntConstructor = assertConstructor(ntClass, "", String.class, int.class,
            int.class);
        Constructor<?> npConstructor = assertConstructor(npClass, "", String.class, int.class,
            int.class);
        Object building;
        Object cmBug;
        Object ntBug;
        Object npBug;
        try {
          building = buildingConstructor.newInstance(1, 5);
          cmBug = cmConstructor.newInstance("ConcurrentModificationBug", 1, 0);
          ntBug = ntConstructor.newInstance("NoneTerminationBug", 2, 0);
          npBug = npConstructor.newInstance("NullPointerBug", 3, 0);
        } catch (InstantiationException | IllegalAccessException | IllegalArgumentException
            | InvocationTargetException e) {
          e.printStackTrace();
          fail(": Setting up failed");
          return;
        }

        try {
          Object[] bugs;
          Object result;

          // Add the bugs
          addBugMethod.invoke(building, cmBug);
          addBugMethod.invoke(building, ntBug);
          addBugMethod.invoke(building, npBug);

          // Move 1: After this [cmBug(0, 3), ntBug(0, 5), npBug(0, 1)]
          bugsMoveMethod.invoke(building);
          assertEquals(5, getConstructionPointsMethod.invoke(building),
              "Incorrect construction points after Move 1");
          result = getAllBugsMethod.invoke(building);
          if (!bugArrayClass.isInstance(result)) {
            fail("Expect to return an array of bugs");
          }
          bugs = (Object[]) result;
          assertEquals(3, bugs.length, ": There should be 3 bugs");
          assertEquals(npBug, bugs[0], "Move 1: The first bug should be NullPointerBug");
          assertEquals(cmBug, bugs[1], "Move 1: The second bug should be ConcurrentModificationBug");
          assertEquals(ntBug, bugs[2], "Move 1: The third bug should be NoneTerminationBug");

          // Move 2: After this [cmBug(0, 2), ntBug(0, 4), npBug(0, 0)]
          bugsMoveMethod.invoke(building);
          assertEquals(5, getConstructionPointsMethod.invoke(building),
              "Incorrect construction points after Move 2");
          result = getAllBugsMethod.invoke(building);
          if (!bugArrayClass.isInstance(result)) {
            fail("Expect to return an array of bugs");
          }
          bugs = (Object[]) result;
          assertEquals(3, bugs.length, ": There should be 3 bugs");
          assertEquals(npBug, bugs[0], "Move 2: The first bug should be NullPointerBug");
          assertEquals(cmBug, bugs[1], "Move 2: The second bug should be ConcurrentModificationBug");
          assertEquals(ntBug, bugs[2], "Move 2: The third bug should be NoneTerminationBug");

          // Move 3: After this [cmBug(0, 1), ntBug(0, 3)], cause 1 damage
          bugsMoveMethod.invoke(building);
          assertEquals(4, getConstructionPointsMethod.invoke(building),
              "Incorrect construction points after Move 3");
          result = getAllBugsMethod.invoke(building);
          if (!bugArrayClass.isInstance(result)) {
            fail("Expect to return an array of bugs");
          }
          bugs = (Object[]) result;
          assertEquals(2, bugs.length, ": There should be 2 bugs");
          assertEquals(cmBug, bugs[0], "Move 3: The first bug should be ConcurrentModificationBug");
          assertEquals(ntBug, bugs[1], "Move 3: The second bug should be NoneTerminationBug");

          // Move 4: After this [cmBug(0, 0), ntBug(0, 2)]
          bugsMoveMethod.invoke(building);
          assertEquals(4, getConstructionPointsMethod.invoke(building),
              "Incorrect construction points after Move 4");
          result = getAllBugsMethod.invoke(building);
          if (!bugArrayClass.isInstance(result)) {
            fail("Expect to return an array of bugs");
          }
          bugs = (Object[]) result;
          assertEquals(2, bugs.length, ": There should be 2 bugs");
          assertEquals(cmBug, bugs[0], "Move 4: The first bug should be ConcurrentModificationBug");
          assertEquals(ntBug, bugs[1], "Move 4: The second bug should be NoneTerminationBug");

          // Move 5: After this [ntBug(0, 1)]
          bugsMoveMethod.invoke(building);
          assertEquals(2, getConstructionPointsMethod.invoke(building),
              "Incorrect construction points after Move 5");
          result = getAllBugsMethod.invoke(building);
          if (!bugArrayClass.isInstance(result)) {
            fail("Expect to return an array of bugs");
          }
          bugs = (Object[]) result;
          assertEquals(1, bugs.length, ": There should be 1 bug");
          assertEquals(ntBug, bugs[0], "Move 5: The first bug should be NoneTerminationBug");

          // Move 6: After this [ntBug(0, 0)]
          bugsMoveMethod.invoke(building);
          assertEquals(2, getConstructionPointsMethod.invoke(building),
              "Incorrect construction points after Move 6");
          result = getAllBugsMethod.invoke(building);
          if (!bugArrayClass.isInstance(result)) {
            fail("Expect to return an array of bugs");
          }
          bugs = (Object[]) result;
          assertEquals(1, bugs.length, ": There should be 1 bug");
          assertEquals(ntBug, bugs[0], "Move 6: The first bug should be NoneTerminationBug");

          // Move 7: After this []
          bugsMoveMethod.invoke(building);
          assertTrue((int) getConstructionPointsMethod.invoke(building) <= 0,
              "Incorrect construction points after Move 7");
          result = getAllBugsMethod.invoke(building);
          if (!bugArrayClass.isInstance(result)) {
            fail("Expect to return an array of bugs");
          }
          bugs = (Object[]) result;
          assertEquals(0, bugs.length, ": There should be no bugs");
        } catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
          e.printStackTrace();
          fail(": Running test failed");
        }

      });
    }

  }


}

