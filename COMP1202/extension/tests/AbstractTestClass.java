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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import org.junit.jupiter.api.BeforeEach;

/**
 * Abstract class for testing signature of the a class.
 *
 * @author htson - v1.0.0 - Initial version
 * @author htson - v1.2.0 - Return the different reflection components, e.g., constructors, methods,
 * fields when asserting. These components will be used in the tests to avoid failed compilation if
 * the elements are not presented.
 * @author htson - v2.0.0 - Remove the different reflection components, as they are unused. Some
 * methods are now static.
 * @version 2.0.0
 */
public abstract class AbstractTestClass {

  // The class under test
  protected Class<?> clazz;

  /**
   * Client should implement this method to return the class under test.
   *
   * @return the class under test.
   */
  protected abstract Class<?> getTestClass();

  /**
   * Before each test, fetch the class under tests and its components.
   */
  @BeforeEach
  public void setup() {
    clazz = this.getTestClass();
  }

  /**
   * Assert that the class under test is a class.
   *
   * @param message the message for testing.
   */
  public void assertClass(String message) {
    assertClass(message, clazz);
  }

  /**
   * Assert that the input class is an interface.
   *
   * @param clazz the input class
   */
  public void assertClass(String message, Class<?> clazz) {
    assertFalse(clazz.isInterface(), message + ": " + clazz + " should be a class");
  }

  /**
   * Assert that there exists a class/interface with the given input name.
   *
   * @param message the message for testing.
   * @param name    the name of the class/interface.
   * @return the class/interface with the given name.
   */
  public static Class<?> assertClassOrInterface(String message, String name) {
    Class<?> clazz = getClassOrInterface(name);
    if (clazz == null) {
      fail(message + ": Cannot find class/interface " + name);
      return null;
    }
    return clazz;
  }

  /**
   * Return the class/interface with the given input name.
   *
   * @param name the name of the class/interface.
   * @return the class/interface with the given name or <code>null</code> if none exists.
   * @since 2.0
   */
  public static Class<?> getClassOrInterface(String name) {
    try {
      return Class.forName(name);
    } catch (ClassNotFoundException e) {
      return null;
    }
  }

  /**
   * Assert that the class under test is an interface.
   *
   * @param message the message for testing.
   */
  public void assertInterface(String message) {
    assertInterface(message, clazz);
  }

  /**
   * Assert that the input class is an interface.
   *
   * @param message the message for testing.
   * @param clazz   the input class
   */
  public static void assertInterface(String message, Class<?> clazz) {
    assertTrue(clazz.isInterface(), message + ": " + clazz + " should be an interface");
  }

  /**
   * Utility method asserting the class under test is an abstract class.
   *
   * @param message the message for testing.
   */
  public void assertAbstract(String message) {
    assertAbstract(message, clazz);
  }

  /**
   * Utility method asserting the input class is an abstract class.
   *
   * @param message the message for testing.
   * @param clazz   the input class
   */
  public static void assertAbstract(String message, Class<?> clazz) {
    assertTrue(Modifier.isAbstract(clazz.getModifiers()),
        message + ": Class " + clazz + " is abstract");
  }

  /**
   * Utility method asserting the direct superclass of the class under test.
   *
   * @param message            the message for testing.
   * @param expectedSuperclass the expected direct superclass of the class under test.
   */
  public void assertSuperclass(String message, Class<?> expectedSuperclass) {
    assertSuperclass(message, clazz, expectedSuperclass);
  }

  /**
   * Utility method asserting the direct superclass of the input class.
   *
   * @param message            the message for testing.
   * @param clazz              the input class.
   * @param expectedSuperclass the expected direct superclass of the class under test.
   */
  public static void assertSuperclass(String message, Class<?> clazz, Class<?> expectedSuperclass) {
    Class<?> actualSuperclass = clazz.getSuperclass();
    assertEquals(expectedSuperclass, actualSuperclass,
        message + ": Incorrect Superclass for " + clazz);
  }

  /**
   * Utility method to check the direct superclass of the class under test.
   *
   * @param expectedSuperclass the expected superclass.
   * @return <code>true</code> if the expected superclass of the class under test.
   * Return <code>false</code> otherwise.
   * @see Class#getSuperclass()
   * @since 2.0
   */
  public boolean isSuperclass(Class<?> expectedSuperclass) {
    return isSuperclass(clazz, expectedSuperclass);
  }

  /**
   * Utility method to check the direct superclass of the input class.
   *
   * @param clazz              the input class.
   * @param expectedSuperclass the expected superclass.
   * @return <code>true</code> if the expected superclass of the class under test.
   * Return <code>false</code> otherwise.
   * @see Class#getSuperclass()
   * @since 2.0
   */
  public static boolean isSuperclass(Class<?> clazz, Class<?> expectedSuperclass) {
    Class<?> actualSuperclass = clazz.getSuperclass();
    if (actualSuperclass == null) {
      return expectedSuperclass == null;
    }
    return actualSuperclass.equals(expectedSuperclass);
  }

  /**
   * Utility method asserting an ancestor of the class under test.
   *
   * @param message               the message for testing.
   * @param expectedAncestorClass the expected ancestor class of the class under test.
   */
  public void assertAncestorClass(String message, Class<?> expectedAncestorClass) {
    assertAncestorClass(message, clazz, expectedAncestorClass);
  }

  /**
   * Utility method asserting an ancestor of the input class.
   *
   * @param message               the message for testing.
   * @param clazz                 the input class
   * @param expectedAncestorClass the expected ancestor class of the class under test.
   * @since 2.0
   */
  public static void assertAncestorClass(String message, Class<?> clazz,
      Class<?> expectedAncestorClass) {
    Set<Class<?>> ancestorClasses = getAllSuperclasses(clazz);
    assertTrue(ancestorClasses.contains(expectedAncestorClass),
        message + ": " + clazz + " is not a subclasses of " + expectedAncestorClass);
  }

  /**
   * Utility method to return all the super classes of the input class. This is done by recursively
   * check through the inheritance hierarchy.
   *
   * @param clazz the input class.
   * @return the ancestor classes.
   * @since 1.1.0
   */
  private static Set<Class<?>> getAllSuperclasses(Class<?> clazz) {
    Set<Class<?>> ancestorClasses = new HashSet<Class<?>>();
    Class<?> superclass = clazz.getSuperclass();
    if (superclass != null) {
      ancestorClasses.add(superclass);
      ancestorClasses.addAll(getAllSuperclasses(superclass));
    }

    return ancestorClasses;
  }

  /**
   * Utility method asserting the class under test implements an interface.
   *
   * @param message                   the message for testing.
   * @param expectedAncestorInterface the expected direct superclass of the class under test.
   */
  public void assertAncestorInterface(String message, Class<?> expectedAncestorInterface) {
    assertAncestorInterface(message, clazz, expectedAncestorInterface);
  }

  /**
   * Utility method asserting the input class implements an interface.
   *
   * @param message                   the message for testing.
   * @param clazz                     the input class
   * @param expectedAncestorInterface the expected direct superclass of the class under test.
   */
  public static void assertAncestorInterface(String message, Class<?> clazz,
      Class<?> expectedAncestorInterface) {
    Set<Class<?>> ancestorInterfaces = getAllInterfaces(clazz);
    assertTrue(ancestorInterfaces.contains(expectedAncestorInterface),
        message + ": " + clazz + " does not implement " + expectedAncestorInterface);
  }

  /**
   * Utility method to return all the super classes of the input class. This is done by recursively
   * check through the inheritance hierarchy.
   *
   * @param clazz the input class.
   * @return the ancestor classes.
   * @since 1.1.0
   */
  private static Set<Class<?>> getAllInterfaces(Class<?> clazz) {
    Set<Class<?>> ancestorInterfaces = new HashSet<Class<?>>();
    Class<?>[] interfaces = clazz.getInterfaces();
    for (Class<?> interfaze : interfaces) {
      ancestorInterfaces.add(interfaze);
      ancestorInterfaces.addAll(getAllInterfaces(interfaze));
    }

    Set<Class<?>> superClasses = getAllSuperclasses(clazz);
    for (Class<?> superclazz : superClasses) {
      ancestorInterfaces.addAll(getAllInterfaces(superclazz));
    }
    return ancestorInterfaces;
  }

  /**
   * Assert a field of a given name for the class under test.
   *
   * @param message the message for testing.
   * @param name    the field name.
   * @return the field with the given name.
   * @see Class#getDeclaredField
   */
  public Field assertDeclaredField(String message, String name) {
    return assertDeclaredField(message, clazz, name);
  }

  /**
   * Assert a field of a given name for the class under test.
   *
   * @param message the message for testing.
   * @param clazz   the input class
   * @param name    the field name.
   * @return the field with the given name.
   * @see Class#getDeclaredField
   * @since 2.0
   */
  public static Field assertDeclaredField(String message, Class<?> clazz, String name) {
    try {
      Field field = clazz.getDeclaredField(name);
      field.setAccessible(true);
      return field;
    } catch (NoSuchFieldException e) {
      fail(message + ": Cannot find the field named " + name + " in class " + clazz.getName());
    } catch (SecurityException e) {
      fail(message + ": Unexpected Security Exception");
    }
    return null;
  }

  /**
   * Assert a field of a given name and type for the class under test.
   *
   * @param message the message for testing.
   * @param name    the field name.
   * @param type    the field type.
   * @return the field matching the input signature.
   */
  public Field assertField(String message, String name, Class<?> type) {
    return assertField(message, type, name, type);
  }

  /**
   * Assert a field of a given name and type for the input class.
   *
   * @param message the message for testing.
   * @param clazz   the input class.
   * @param name    the field name.
   * @param type    the field type.
   * @return the field matching the input signature.
   * @since 2.0
   */
  public static Field assertField(String message, Class<?> clazz, String name, Class<?> type) {
    try {
      Field actualField = clazz.getDeclaredField(name);
      Class<?> actualType = actualField.getType();
      assertEquals(type, actualType, message + ": Incorrect type for " + name);
      actualField.setAccessible(true);
      return actualField;
    } catch (NoSuchFieldException e) {
      fail(message + ": Cannot find the field named " + name + " in class " + clazz.getName());
    } catch (SecurityException e) {
      fail(message + ": Unexpected Security Exception");
    }
    return null;
  }

  /**
   * Assert a private field of a given name for the class under test.
   *
   * @param message the message for testing.
   * @param name    the field name
   * @return the field matching the input signature.
   */
  public Field assertPrivateField(String message, String name) {
    return assertPrivateField(message, clazz, name);
  }

  /**
   * Assert a private field of a given name for the input class.
   *
   * @param message the message for testing.
   * @param clazz   the input class.
   * @param name    the field name
   * @return the field matching the input signature.
   * @since 2.0
   */
  public static Field assertPrivateField(String message, Class<?> clazz, String name) {
    try {
      Field actualField = clazz.getDeclaredField(name);
      assertTrue(Modifier.isPrivate(actualField.getModifiers()),
          message + ": Field " + name + " is private");
      return actualField;
    } catch (NoSuchFieldException e) {
      fail(message + ": Cannot find the field named " + name + " in class " + clazz.getName());
    } catch (SecurityException e) {
      fail(message + ": Unexpected Security Exception");
    }
    return null;
  }

  /**
   * Assert a private field of a given name and type for the class under test.
   *
   * @param message the message for testing.
   * @param name    the field name
   * @param type    the field type
   * @return the field matching the input signature.
   */
  public Field assertPrivateField(String message, String name, Class<?> type) {
    return assertPrivateField(message, type, name, type);
  }

  /**
   * Assert a private field of a given name and type for the input class.
   *
   * @param message the message for testing.
   * @param clazz   the input class.
   * @param name    the field name
   * @param type    the field type
   * @return the field matching the input signature.
   * @since 2.0
   */
  public static Field assertPrivateField(String message, Class<?> clazz, String name,
      Class<?> type) {
    try {
      Field actualField = clazz.getDeclaredField(name);
      Class<?> actualType = actualField.getType();
      assertEquals(type, actualType, message + ": Incorrect type for " + name);
      assertTrue(Modifier.isPrivate(actualField.getModifiers()),
          message + ": Field " + name + " is abstract");
      actualField.setAccessible(true);
      return actualField;
    } catch (NoSuchFieldException e) {
      fail(message + ": Cannot find the field named " + name + " in class " + clazz.getName());
    } catch (SecurityException e) {
      fail(message + ": Unexpected Security Exception");
    }
    return null;
  }

  /**
   * Assert and return the constructor with the given parameter types for the class under test.
   *
   * @param message        the message for testing.
   * @param parameterTypes the parameter types
   * @return the constructor that match the input signature.
   */
  public Constructor<?> assertConstructor(String message, Class<?>... parameterTypes) {
    return assertConstructor(clazz, message, parameterTypes);
  }

  /**
   * Assert and return the constructor with the given parameter types for an input class.
   *
   * @param clazz          the class under test
   * @param message        the message for testing.
   * @param parameterTypes the parameter types
   * @return the constructor that match the input signature.
   */
  public static Constructor<?> assertConstructor(Class<?> clazz, String message,
      Class<?>... parameterTypes) {
    try {
      Constructor<?> constructor = clazz.getDeclaredConstructor(parameterTypes);
      constructor.setAccessible(true);
      return constructor;
    } catch (NoSuchMethodException e) {
      fail(message + ": Cannot find constructor for " + clazz + " with parameter types "
          + Arrays.toString(parameterTypes));
    } catch (SecurityException e) {
      fail(message + ": Unexpected Security Exception");
    }
    return null;
  }

  /**
   * Assert and return a method with a given signature for the class under test. The method chosen
   * will be the most specific one that matches the required signature in the class hierarchy.
   *
   * @param message        the message for testing.
   * @param name           the name of the method.
   * @param parameterTypes the parameter types.
   * @return the method matching the input signature.
   * @see #assertMethod(Class, String, String, Class[])
   */
  public Method assertMethod(String message, String name, Class<?>... parameterTypes) {
    return assertMethod(clazz, message, name, parameterTypes);
  }

  /**
   * Assert and return a method with a given signature for an input class. The method chosen will be
   * the most specific one that matches the required signature in the class hierarchy.
   *
   * @param clazz          the input class.
   * @param message        the message for testing.
   * @param name           the name of the method.
   * @param parameterTypes the parameter types.
   * @return the method matching the input signature.
   * @see #assertMethod(Class, String, Class, String, Class[])
   */
  public static Method assertMethod(Class<?> clazz, String message, String name,
      Class<?>... parameterTypes) {
    return assertMethod(clazz, message, null, name, parameterTypes);
  }

  /**
   * Assert and return a method with a given signature for the class under test. The method chosen
   * will be the most specific one that matches the required signature in the class hierarchy.
   *
   * @param message        the message for testing.
   * @param returnType     the expected return type for the method.
   * @param name           the name of the method.
   * @param parameterTypes the parameter types.
   * @return the method matching the input signature.
   * @see #assertMethod(Class, String, Class, String, Class[])
   */
  public Method assertMethod(String message, Class<?> returnType, String name,
      Class<?>... parameterTypes) {
    return assertMethod(clazz, message, returnType, name, parameterTypes);
  }

  /**
   * Assert and return a method with a given signature for an input class. The method chosen will be
   * the most specific one that matches the required signature in the class hierarchy.
   *
   * @param clazz          the class under test.
   * @param message        the message for testing.
   * @param returnType     the expected return type for the method.
   * @param name           the name of the method.
   * @param parameterTypes the parameter types.
   * @return the method matching the input signature.
   * @see Class#getMethod(String, Class[])
   */
  public static Method assertMethod(Class<?> clazz, String message, Class<?> returnType,
      String name,
      Class<?>... parameterTypes) {
    try {
      Method actualMethod = clazz.getMethod(name, parameterTypes);
      if (returnType != null) {
        assertEquals(returnType, actualMethod.getReturnType(),
            message + ": Incorrect return type for method "
                + name + " with parameter types " + Arrays.toString(parameterTypes));
      }
      actualMethod.setAccessible(true);
      return actualMethod;
    } catch (NoSuchMethodException e) {
      fail(message + ": Cannot find method " + name + " with parameter types " + Arrays.toString(
          parameterTypes));
    } catch (SecurityException e) {
      fail(message + ": Unexpected Security Exception");
    }
    return null;
  }

  /**
   * Assert and return a method with a given signature for an input class. The method chosen will be
   * the most specific one that matches the required signature in the class hierarchy.
   *
   * @param message          the message for testing.
   * @param clazz            the input class.
   * @param expectedModifier the modifier of the method.
   * @param returnType       the return type for the method.
   * @param name             the name of the method.
   * @param parameterTypes   the parameter types.
   * @return the method matching the input signature.
   */
  public static Method assertMethod(String message, Class<?> clazz, int expectedModifier,
      Class<?> returnType,
      String name, Class<?>... parameterTypes) {
    try {
      Method actualMethod = clazz.getDeclaredMethod(name, parameterTypes);
      assertEquals(returnType, actualMethod.getReturnType(),
          message + ": Incorrect return type for method " + name + " with parameter types "
              + parameterTypes);
      assertEquals(expectedModifier, actualMethod.getModifiers(), message
          + ": Incorrect access modifier for method " + name + " with parameter types "
          + parameterTypes);
      actualMethod.setAccessible(true);
      return actualMethod;
    } catch (NoSuchMethodException e) {
      fail(message + ": Cannot find method " + name + " with parameter types " + parameterTypes);
    } catch (SecurityException e) {
      fail(message + ": Unexpected Security Exception");
    }
    return null;
  }

  /**
   * Assert and return a method with a given signature for the class under test. The method chosen
   * will be the most specific one that matches the required signature in the class hierarchy.
   *
   * @param message          the message for testing.
   * @param expectedModifier the modifier of the method.
   * @param returnType       the return type for the method.
   * @param name             the name of the method.
   * @param parameterTypes   the parameter types.
   * @return the method matching the input signature.
   */
  public Method assertMethod(String message, int expectedModifier, Class<?> returnType, String name,
      Class<?>... parameterTypes) {
    try {
      Method actualMethod = clazz.getDeclaredMethod(name, parameterTypes);
      assertEquals(returnType, actualMethod.getReturnType(),
          message + ": Incorrect return type for method " + name + " with parameter types "
              + parameterTypes);
      assertEquals(expectedModifier, actualMethod.getModifiers(), message
          + ": Incorrect access modifier for method " + name + " with parameter types "
          + parameterTypes);
      return actualMethod;
    } catch (NoSuchMethodException e) {
      fail(message + ": Cannot find method " + name + " with parameter types " + parameterTypes);
    } catch (SecurityException e) {
      fail(message + ": Unexpected Security Exception");
    }
    return null;
  }

  /**
   * Assert and return an accessible method with a given signature for the class under test.
   *
   * @param message        the message for testing.
   * @param name           the name of the method.
   * @param parameterTypes the parameter types.
   * @return the method matching the input signature.
   * @see #getAccessibleMethod(Class, String, Class...)
   */
  public Method assertAccessibleMethod(String message, String name, Class<?>... parameterTypes) {
    return assertAccessibleMethod(clazz, message, name, parameterTypes);
  }

  /**
   * Assert and return an accessible method with a given signature for an input class.
   *
   * @param clazz          the input class.
   * @param message        the message for testing.
   * @param name           the name of the method.
   * @param parameterTypes the parameter types.
   * @return the method matching the input signature.
   * @see #getAccessibleMethod(Class, String, Class...)
   */
  public static Method assertAccessibleMethod(Class<?> clazz, String message, String name,
      Class<?>... parameterTypes) {
    return assertAccessibleMethod(clazz, message, null, name, parameterTypes);
  }

  /**
   * Assert and return an accessible method with a given signature for the class under test.
   *
   * @param message        the message for testing.
   * @param returnType     the expected return type for the method.
   * @param name           the name of the method.
   * @param parameterTypes the parameter types.
   * @return the method matching the input signature.
   * @see #getAccessibleMethod(Class, String, Class...)
   */
  public Method assertAccessibleMethod(String message, Class<?> returnType, String name,
      Class<?>... parameterTypes) {
    return assertAccessibleMethod(clazz, message, returnType, name, parameterTypes);
  }

  /**
   * Assert and return an accessible method with a given signature for an input class.
   *
   * @param clazz          the input class.
   * @param message        the message for testing.
   * @param returnType     the expect return type for the method.
   * @param name           the name of the method.
   * @param parameterTypes the parameter types.
   * @return the method matching the input signature.
   * @see #getAccessibleMethod(Class, String, Class...)
   */
  public static Method assertAccessibleMethod(Class<?> clazz, String message, Class<?> returnType,
      String name,
      Class<?>... parameterTypes) {
    try {
      Method actualMethod = getAccessibleMethod(clazz, name, parameterTypes);
      assertNotNull(actualMethod,
          message + ": Cannot find method " + name + " with parameter types "
              + Arrays.toString(parameterTypes) + " for " + clazz);
      if (returnType != null) {
        assertEquals(returnType, actualMethod.getReturnType(),
            message + ": Incorrect return type for method "
                + name + " with parameter types " + Arrays.toString(parameterTypes));
      }
      actualMethod.setAccessible(true);
      return actualMethod;
    } catch (NoSuchMethodException e) {
      fail(message + ": Cannot find method " + name + " with parameter types " + Arrays.toString(
          parameterTypes)
          + " for " + clazz);
    } catch (SecurityException e) {
      fail(message + ": Unexpected Security Exception");
    }
    return null;
  }

  /**
   * Utility method to get an accessible method of an input class with the input signature.
   *
   * @param clazz          the input class
   * @param name           the name of the method
   * @param parameterTypes the input parameter types
   * @return the accessible method matching the input signature.
   * @throws SecurityException     if unexpected security exception in getting declared method.
   * @throws NoSuchMethodException if no accessible method with the matching signature is found.
   * @see Class#getDeclaredMethod(String, Class...)
   */
  private static Method getAccessibleMethod(Class<?> clazz, String name, Class<?>... parameterTypes)
      throws SecurityException, NoSuchMethodException {
    try {
      return clazz.getDeclaredMethod(name, parameterTypes);
    } catch (NoSuchMethodException e) {
      Class<?> superclass = clazz.getSuperclass();
      if (superclass == null) {
        throw e;
      }
      // Try to look at the superclass
      return getIndirectAccessibleMethod(superclass, name, parameterTypes);
    }
  }

  /**
   * Utility method to get an indirect accessible method of an input class with the input
   * signature.
   *
   * @param clazz          the input class
   * @param name           the name of the method
   * @param parameterTypes the input parameter types
   * @return the accessible method matching the input signature.
   * @throws SecurityException     if unexpected security exception in getting declared method.
   * @throws NoSuchMethodException if no accessible method with the matching signature is found.
   * @see Class#getDeclaredMethod(String, Class...)
   */
  private static Method getIndirectAccessibleMethod(Class<?> clazz, String name,
      Class<?>... parameterTypes)
      throws SecurityException, NoSuchMethodException {
    try {
      Method declaredMethod = clazz.getDeclaredMethod(name, parameterTypes);
      if (Modifier.isPrivate(declaredMethod.getModifiers())) {
        return null;
      }
      return declaredMethod;
    } catch (NoSuchMethodException e) {
      Class<?> superclass = clazz.getSuperclass();
      if (superclass == null) {
        throw e;
      }
      // Try to look at the superclass
      return getIndirectAccessibleMethod(superclass, name, parameterTypes);
    }
  }

}
