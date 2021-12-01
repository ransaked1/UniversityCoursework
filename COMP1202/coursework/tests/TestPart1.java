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

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

/**
 * Unit Tests Coursework signature
 *
 * @author htson - v1.0 - Initial API and implementation
 * @version 1.0
 */
public class TestPart1 extends TestCoursework {

  /**
   * Test class for {@link bugs.Bug}'s signature.
   *
   * @author htson
   * @version 1.0
   * @see TestCoursework.AbstractTestBug
   */
  @Nested
  public class TestBugSignature extends AbstractTestBug {

    @Test
    @DisplayName("Test bugs.Bug's signature")
    public void testBug_Signature() {
      assertClass("");
      assertDeclaredField("", "name");
      assertDeclaredField("", "baseHp");
      assertDeclaredField("", "baseSteps");
      assertDeclaredField("", "level");
      assertConstructor("", String.class, int.class, int.class, int.class);
      assertMethod("", "getBaseSteps");
      assertMethod("", "getLevel");
      assertDeclaredField("", "currentHp");
      assertDeclaredField("", "currentSteps");
      assertDeclaredField("", "currentFloor");
      assertConstructor("", String.class, int.class, int.class, int.class, int.class);
      assertMethod("", "getCurrentHp");
      assertMethod("", "getCurrentSteps");
      assertMethod("", "getCurrentFloor");
      assertMethod("", "move");
      assertMethod("", "damage", int.class);
      assertMethod("", "slowDown", int.class);
    }
  }

  /**
   * Test class for {@link bugs.Bug}'s specification. We use the concrete classes bugs.NullPointerBug to
   * test.
   *
   * @author htson
   * @version 1.0
   * @see TestCoursework.AbstractTestNullPointerBug
   */
  @Nested
  public class TestBugSpecification extends AbstractTestNullPointerBug {

    private Class<?> bugClass;

    @BeforeEach
    public void setup() {
      super.setup();
      bugClass = getClassOrInterface("bugs.Bug");
      assertClass("", bugClass);
    }

    @Test
    @DisplayName("Test bugs.Bug's name and related methods")
    public void testBug_Name() {
      assertTimeout(ofMillis(1000), () -> {
        Constructor<?> npConstructor = assertConstructor("", String.class, int.class, int.class);
        Field nameMethod = assertDeclaredField("", bugClass, "name");
        Object npBug;
        try {
          npBug = npConstructor.newInstance("Bug", 1, 1);
        } catch (InstantiationException | IllegalAccessException | IllegalArgumentException
            | InvocationTargetException e) {
          e.printStackTrace();
          fail(": Setting up failed");
          return;
        }
        try {
          assertEquals("Bug", nameMethod.get(npBug), ": Incorrect name");
        } catch (IllegalAccessException | IllegalArgumentException e) {
          e.printStackTrace();
          fail(": Running test failed");
        }
      });
    }

    @Test
    @DisplayName("Test bugs.Bug's currentHp and related methods")
    public void testBug_currentHp() {
      assertTimeout(ofMillis(1000), () -> {
        Constructor<?> npConstructor = assertConstructor("", String.class, int.class, int.class);
        Method getCurrentHpMethod = assertMethod(bugClass, "", "getCurrentHp");
        Object npBug;
        try {
          npBug = npConstructor.newInstance("Bug", 2, 1);
        } catch (InstantiationException | IllegalAccessException | IllegalArgumentException
            | InvocationTargetException e) {
          e.printStackTrace();
          fail(": Setting up failed");
          return;
        }
        try {
          assertEquals(28, getCurrentHpMethod.invoke(npBug),
              ": Incorrect current HP for NullPointerBug Level 2");
        } catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
          e.printStackTrace();
          fail(": Running test failed");
        }
      });
    }

    @Test
    @DisplayName("Test bugs.Bug's currentSteps and related methods")
    public void testBug_currentSteps() {
      assertTimeout(ofMillis(1000), () -> {
        Constructor<?> npConstructor = assertConstructor("", String.class, int.class, int.class);
        Method getCurrentStepsMethod = assertMethod(bugClass, "", "getCurrentSteps");
        Object npBug;
        try {
          npBug = npConstructor.newInstance("Bug", 2, 1);
        } catch (InstantiationException | IllegalAccessException | IllegalArgumentException
            | InvocationTargetException e) {
          e.printStackTrace();
          fail(": Setting up failed");
          return;
        }
        try {
          assertEquals(1, getCurrentStepsMethod.invoke(npBug),
              ": Incorrect current steps for NullPointerBug with 1 initial step");
        } catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
          e.printStackTrace();
          fail(": Running test failed");
        }
      });
    }

    @Test
    @DisplayName("Test bugs.Bug's currentFloor and related methods")
    public void testBug_currentFloor() {
      assertTimeout(ofMillis(1000), () -> {
        Constructor<?> npConstructor = assertConstructor("", String.class, int.class, int.class);
        Method getCurrentFloorMethod = assertMethod(bugClass, "", "getCurrentFloor");
        Object npBug;
        try {
          npBug = npConstructor.newInstance("Bug", 2, 1);
        } catch (InstantiationException | IllegalAccessException | IllegalArgumentException
            | InvocationTargetException e) {
          e.printStackTrace();
          fail(": Setting up failed");
          return;
        }
        try {
          assertEquals(-1, getCurrentFloorMethod.invoke(npBug),
              ": Incorrect initial current floor for NullPointerBug");
        } catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
          e.printStackTrace();
          fail(": Running test failed");
        }
      });
    }

    @Test
    @DisplayName("Test bugs.Bug's move method")
    public void testBug_move() {
      assertTimeout(ofMillis(1000), () -> {
        Constructor<?> npConstructor = assertConstructor("", String.class, int.class, int.class);
        Method moveMethod = assertMethod(bugClass, "", "move");
        Method getCurrentFloorMethod = assertMethod(bugClass, "", "getCurrentFloor");
        Method getCurrentStepsMethod = assertMethod(bugClass, "", "getCurrentSteps");
        Object npBug;
        try {
          npBug = npConstructor.newInstance("Bug", 2, 1);
        } catch (InstantiationException | IllegalAccessException | IllegalArgumentException
            | InvocationTargetException e) {
          e.printStackTrace();
          fail(": Setting up failed");
          return;
        }
        try {
          moveMethod.invoke(npBug);
          assertEquals(0, getCurrentStepsMethod.invoke(npBug),
              ": Incorrect current steps for NullPointerBug after 1 move");
          assertEquals(-1, getCurrentFloorMethod.invoke(npBug),
              ": Incorrect current floor for NullPointerBug after 1 move");
          moveMethod.invoke(npBug);
          assertEquals(1, getCurrentStepsMethod.invoke(npBug),
              ": Incorrect current steps for NullPointerBug after 2 moves");
          assertEquals(0, getCurrentFloorMethod.invoke(npBug),
              ": Incorrect current floor for NullPointerBug after 2 moves");
          moveMethod.invoke(npBug);
          assertEquals(0, getCurrentStepsMethod.invoke(npBug),
              ": Incorrect current steps for NullPointerBug after 3 moves");
          assertEquals(0, getCurrentFloorMethod.invoke(npBug),
              ": Incorrect current floor for NullPointerBug after 3 moves");
        } catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
          e.printStackTrace();
          fail(": Running test failed");
        }
      });
    }

    @Test
    @DisplayName("Test bugs.Bug's damage method")
    public void testBug_damage() {
      assertTimeout(ofMillis(1000), () -> {
        Constructor<?> npConstructor = assertConstructor("", String.class, int.class, int.class);
        Method damageMethod = assertMethod(bugClass, "", "damage", int.class);
        Method getCurrentHpMethod = assertMethod(bugClass, "", "getCurrentHp");
        Object npBug;
        try {
          npBug = npConstructor.newInstance("Bug", 1, 1);
        } catch (InstantiationException | IllegalAccessException | IllegalArgumentException
            | InvocationTargetException e) {
          e.printStackTrace();
          fail(": Setting up failed");
          return;
        }
        try {
          damageMethod.invoke(npBug, 6);
          assertEquals(4, getCurrentHpMethod.invoke(npBug),
              ": Incorrect current steps for NullPointerBug after 1 damage of 6");
          damageMethod.invoke(npBug, 3);
          assertEquals(1, getCurrentHpMethod.invoke(npBug),
              ": Incorrect current steps for NullPointerBug after 2 damages of (6+3)");
          damageMethod.invoke(npBug, 2);
          assertEquals(0, getCurrentHpMethod.invoke(npBug),
              ": Incorrect current steps for NullPointerBug after 3 damages of (6+3+2)");
        } catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
          e.printStackTrace();
          fail(": Running test failed");
        }
      });
    }

    @Test
    @DisplayName("Test bugs.Bug's slowDown method")
    public void testBug_slowDown() {
      assertTimeout(ofMillis(1000), () -> {
        Constructor<?> npConstructor = assertConstructor("", String.class, int.class, int.class);
        Method slowDownMethod = assertMethod(bugClass, "", "slowDown", int.class);
        Method getCurrentStepsMethod = assertMethod(bugClass, "", "getCurrentSteps");
        Object npBug;
        try {
          npBug = npConstructor.newInstance("Bug", 1, 1);
        } catch (InstantiationException | IllegalAccessException | IllegalArgumentException
            | InvocationTargetException e) {
          e.printStackTrace();
          fail(": Setting up failed");
          return;
        }
        try {
          slowDownMethod.invoke(npBug, 2);
          assertEquals(3, getCurrentStepsMethod.invoke(npBug),
              ": Incorrect current steps for NullPointerBug (initial 1 step) after 1 slowdown of 2");
          slowDownMethod.invoke(npBug, 1);
          assertEquals(4, getCurrentStepsMethod.invoke(npBug),
              ": Incorrect current steps for NullPointerBug (initial 1 step) after 1 slowdown of (2+1)");
        } catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
          e.printStackTrace();
          fail(": Running test failed");
        }
      });
    }
  }

  /**
   * Test class for {@link bugs.ConcurrentModificationBug}'s signature.
   *
   * @author htson
   * @version 1.0
   * @see TestCoursework.AbstractTestConcurrentModificationBug
   */
  @Nested
  public class TestConcurrentModificationBugSignature extends
      AbstractTestConcurrentModificationBug {

    @Test
    @DisplayName("Test bugs.ConcurrentModificationBug's signature")
    public void testConcurrentModificationBug_Signature() {
      assertClass("");
      assertConstructor("", String.class, int.class, int.class);
    }
  }

  /**
   * Test class for {@link bugs.ConcurrentModificationBug}'s specification.
   *
   * @author htson
   * @version 1.0
   * @see TestCoursework.AbstractTestConcurrentModificationBug
   */
  @Nested
  public class TestConcurrentModificationBugSpecification extends
      AbstractTestConcurrentModificationBug {

    private Class<?> bugClass;

    @BeforeEach
    public void setup() {
      super.setup();
      bugClass = getClassOrInterface("bugs.Bug");
      assertClass("", bugClass);
    }

    @Test
    @DisplayName("Test bugs.ConcurrentModificationBug's constructor")
    public void testConcurrentModificationBug_constructor() {
      assertTimeout(ofMillis(1000), () -> {
        Constructor<?> cmConstructor = assertConstructor("", String.class, int.class, int.class);
        Field nameField = assertDeclaredField("", bugClass, "name");
        Method getBaseStepsMethod = assertMethod(bugClass, "", "getBaseSteps");
        Method getLevelMethod = assertMethod(bugClass, "", "getLevel");
        Method getCurrentHpMethod = assertMethod(bugClass, "", "getCurrentHp");
        Method getCurrentStepsMethod = assertMethod(bugClass, "", "getCurrentSteps");
        Method getCurrentFloorMethod = assertMethod(bugClass, "", "getCurrentFloor");
        Object cmBug;
        try {
          cmBug = cmConstructor.newInstance("ConcurrentModificationBug", 2, 2);
        } catch (InstantiationException | IllegalAccessException | IllegalArgumentException
            | InvocationTargetException e) {
          e.printStackTrace();
          fail(": Setting up failed");
          return;
        }
        try {
          assertEquals("ConcurrentModificationBug", nameField.get(cmBug), ": Incorrect name");
          assertEquals(4, getBaseStepsMethod.invoke(cmBug),
              ": Incorrect base steps for ConcurrentModificationBug");
          assertEquals(2, getLevelMethod.invoke(cmBug),
              ": Incorrect base steps for ConcurrentModificationBug");
          assertEquals(57, getCurrentHpMethod.invoke(cmBug),
              ": Incorrect current HP for ConcurrentModificationBug level 2");
          assertEquals(2, getCurrentStepsMethod.invoke(cmBug),
              ": Incorrect current steps for ConcurrentModificationBug with 2 steps initially");
          assertEquals(-1, getCurrentFloorMethod.invoke(cmBug),
              ": Incorrect current floor for ConcurrentModificationBug initially");
        } catch (IllegalAccessException | IllegalArgumentException e) {
          e.printStackTrace();
          fail(": Running test failed");
        }
      });
    }

  }

  /**
   * Test class for {@link bugs.NoneTerminationBug}'s signature.
   *
   * @author htson
   * @version 1.0
   * @see TestCoursework.AbstractTestNoneTerminationBug
   */
  @Nested
  public class TestNoneTerminationBugSignature extends AbstractTestNoneTerminationBug {

    @Test
    @DisplayName("Test bugs.NoneTerminationBug's signature")
    public void testNoneTerminationBug_Signature() {
      assertClass("");
      assertConstructor("", String.class, int.class, int.class);
    }
  }

  /**
   * Test class for {@link bugs.NoneTerminationBug}'s specification.
   *
   * @author htson
   * @version 1.0
   * @see TestCoursework.AbstractTestNoneTerminationBug
   */
  @Nested
  public class TestNoneTerminationBugSpecification extends
      AbstractTestNoneTerminationBug {

    private Class<?> bugClass;

    @BeforeEach
    public void setup() {
      super.setup();
      bugClass = getClassOrInterface("bugs.Bug");
      assertClass("", bugClass);
    }

    @Test
    @DisplayName("Test bugs.NoneTerminationBug's constructor")
    public void testNoneTerminationBug_constructor() {
      assertTimeout(ofMillis(1000), () -> {
        Constructor<?> ntConstructor = assertConstructor("", String.class, int.class, int.class);
        Field nameField = assertDeclaredField("", bugClass, "name");
        Method getBaseStepsMethod = assertMethod(bugClass, "", "getBaseSteps");
        Method getLevelMethod = assertMethod(bugClass, "", "getLevel");
        Method getCurrentHpMethod = assertMethod(bugClass, "", "getCurrentHp");
        Method getCurrentStepsMethod = assertMethod(bugClass, "", "getCurrentSteps");
        Method getCurrentFloorMethod = assertMethod(bugClass, "", "getCurrentFloor");
        Object ntBug;
        try {
          ntBug = ntConstructor.newInstance("NoneTerminationBug", 3, 3);
        } catch (InstantiationException | IllegalAccessException | IllegalArgumentException
            | InvocationTargetException e) {
          e.printStackTrace();
          fail(": Setting up failed");
          return;
        }
        try {
          assertEquals("NoneTerminationBug", nameField.get(ntBug), ": Incorrect name");
          assertEquals(6, getBaseStepsMethod.invoke(ntBug),
              ": Incorrect base steps for NoneTerminationBug");
          assertEquals(3, getLevelMethod.invoke(ntBug),
              ": Incorrect base steps for NoneTerminationBug");
          assertEquals(1039, getCurrentHpMethod.invoke(ntBug),
              ": Incorrect current HP for NoneTerminationBug level 3");
          assertEquals(3, getCurrentStepsMethod.invoke(ntBug),
              ": Incorrect current steps for NoneTerminationBug with 3 steps initially");
          assertEquals(-1, getCurrentFloorMethod.invoke(ntBug),
              ": Incorrect current floor for NoneTerminationBug initially");
        } catch (IllegalAccessException | IllegalArgumentException e) {
          e.printStackTrace();
          fail(": Running test failed");
        }
      });
    }

  }

  /**
   * Test class for {@link bugs.NullPointerBug}'s signature.
   *
   * @author htson
   * @version 1.0
   * @see TestCoursework.AbstractTestNullPointerBug
   */
  @Nested
  public class TestNullPointerBugSignature extends AbstractTestNullPointerBug {

    @Test
    @DisplayName("Test bugs.NullPointerBug's signature")
    public void testNullPointerBug_Signature() {
      assertClass("");
      assertConstructor("", String.class, int.class, int.class);
    }
  }

  /**
   * Test class for {@link bugs.NullPointerBug}'s specification.
   *
   * @author htson
   * @version 1.0
   * @see TestCoursework.AbstractTestNullPointerBug
   */
  @Nested
  public class TestNullPointerBugSpecification extends
      AbstractTestNullPointerBug {

    private Class<?> bugClass;

    @BeforeEach
    public void setup() {
      super.setup();
      bugClass = getClassOrInterface("bugs.Bug");
      assertClass("", bugClass);
    }

    @Test
    @DisplayName("Test bugs.NullPointerBug's constructor")
    public void testNullPointerBug_constructor() {
      assertTimeout(ofMillis(1000), () -> {
        Constructor<?> ntConstructor = assertConstructor("", String.class, int.class, int.class);
        Field nameField = assertDeclaredField("", bugClass, "name");
        Method getBaseStepsMethod = assertMethod(bugClass, "", "getBaseSteps");
        Method getLevelMethod = assertMethod(bugClass, "", "getLevel");
        Method getCurrentHpMethod = assertMethod(bugClass, "", "getCurrentHp");
        Method getCurrentStepsMethod = assertMethod(bugClass, "", "getCurrentSteps");
        Method getCurrentFloorMethod = assertMethod(bugClass, "", "getCurrentFloor");
        Object ntBug;
        try {
          ntBug = ntConstructor.newInstance("NullPointerBug", 4, 4);
        } catch (InstantiationException | IllegalAccessException | IllegalArgumentException
            | InvocationTargetException e) {
          e.printStackTrace();
          fail(": Setting up failed");
          return;
        }
        try {
          assertEquals("NullPointerBug", nameField.get(ntBug), ": Incorrect name");
          assertEquals(2, getBaseStepsMethod.invoke(ntBug),
              ": Incorrect base steps for NullPointerBug");
          assertEquals(4, getLevelMethod.invoke(ntBug),
              ": Incorrect base steps for NullPointerBug");
          assertEquals(80, getCurrentHpMethod.invoke(ntBug),
              ": Incorrect current HP for NullPointerBug level 4");
          assertEquals(4, getCurrentStepsMethod.invoke(ntBug),
              ": Incorrect current steps for NullPointerBug with 4 steps initially");
          assertEquals(-1, getCurrentFloorMethod.invoke(ntBug),
              ": Incorrect current floor for NullPointerBug initially");
        } catch (IllegalAccessException | IllegalArgumentException e) {
          e.printStackTrace();
          fail(": Running test failed");
        }
      });
    }

  }

}
