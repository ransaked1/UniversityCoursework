package students;

import building.*;
import bugs.*;

public interface Student {
  public int getLevel();

  public int getDamage();

  public int getDelay();

  public int getDelayCounter();

  public int upgradeCost();

  public int levelUpDamage();

  public int defence(Building building);

  public void upgrade();
}
