package students;

import building.*;
import bugs.*;

interface Student {
  public int getLevel();

  public int upgradeCost();

  public int defence(Building building);
}
