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

public abstract class TestCoursework {

  /**
   * Abstract test class for {@link bugs.Bug}.
   *
   * @author htson
   * @version 1.0
   * @see AbstractTestClass
   */
  protected abstract static class AbstractTestBug extends AbstractTestClass {

    /**
     * We are testing {@link bugs.Bug}.
     */
    protected Class<?> getTestClass() {
      return assertClassOrInterface("", "bugs.Bug");
    }
  }

  /**
   * Abstract test class for {@link bugs.ConcurrentModificationBug}.
   *
   * @author htson
   * @version 1.0
   * @see AbstractTestClass
   */
  protected abstract static class AbstractTestConcurrentModificationBug extends AbstractTestClass {

    protected Class<?> getTestClass() {
      return assertClassOrInterface("", "bugs.ConcurrentModificationBug");
    }
  }

  /**
   * Abstract test class for {@link bugs.NullPointerBug}.
   *
   * @author htson
   * @version 1.0
   * @see AbstractTestClass
   */
  protected abstract static class AbstractTestNullPointerBug extends AbstractTestClass {

    protected Class<?> getTestClass() {
      return assertClassOrInterface("", "bugs.NullPointerBug");
    }
  }

  /**
   * Abstract test class for {@link bugs.NoneTerminationBug}.
   *
   * @author htson
   * @version 1.0
   * @see AbstractTestClass
   */
  protected abstract static class AbstractTestNoneTerminationBug extends AbstractTestClass {

    protected Class<?> getTestClass() {
      return assertClassOrInterface("", "bugs.NoneTerminationBug");
    }
  }

  /**
   * Abstract test class for {@link building.Building}.
   *
   * @author htson
   * @version 1.0
   * @see AbstractTestClass
   */
  protected abstract static class AbstractTestBuilding extends AbstractTestClass {

    protected Class<?> getTestClass() {
      return assertClassOrInterface("", "building.Building");
    }
  }

  /**
   * Abstract test class for {@link students.Student}.
   *
   * @author htson
   * @version 1.0
   * @see AbstractTestClass
   */
  protected abstract static class AbstractTestStudent extends AbstractTestClass {

    protected Class<?> getTestClass() {
      return assertClassOrInterface("", "students.Student");
    }
  }

  /**
   * Abstract test class for {@link students.AiStudent}.
   *
   * @author htson
   * @version 1.0
   * @see AbstractTestClass
   */
  protected abstract static class AbstractTestAiStudent extends AbstractTestClass {

    protected Class<?> getTestClass() {
      return assertClassOrInterface("", "students.AiStudent");
    }
  }


  /**
   * Abstract test class for {@link students.CsStudent}.
   *
   * @author htson
   * @version 1.0
   * @see AbstractTestClass
   */
  protected abstract static class AbstractTestCsStudent extends AbstractTestClass {

    protected Class<?> getTestClass() {
      return assertClassOrInterface("", "students.CsStudent");
    }
  }

  /**
   * Abstract test class for {@link students.CyberStudent}.
   *
   * @author htson
   * @version 1.0
   * @see AbstractTestClass
   */
  protected abstract static class AbstractTestCyberStudent extends AbstractTestClass {

    protected Class<?> getTestClass() {
      return assertClassOrInterface("", "students.CyberStudent");
    }
  }

  /**
   * Abstract test class for {@link students.SeStudent}.
   *
   * @author htson
   * @version 1.0
   * @see AbstractTestClass
   */
  protected abstract static class AbstractTestSeStudent extends AbstractTestClass {

    protected Class<?> getTestClass() {
      return assertClassOrInterface("", "students.SeStudent");
    }
  }

}
