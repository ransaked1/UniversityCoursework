package students;

import bugs.*;
import building.*;

/**
 * Student interface specifying signatures for obligatory functions to be implemented in a Student
 * object.
 */
public interface Student {
  // Specification methods for the interface
  public int getLevel();

  public int upgradeCost();

  public int defence(Building building);

  // Additional method signatures
  public int getDamage();

  public int getDelay();

  public int getDelayCounter();

  public int levelUpDamage();

  public void upgrade();
}
