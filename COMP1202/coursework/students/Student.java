package students;

import building.*;
import bugs.*;

public interface Student {
  public int getLevel();

  public int upgradeCost();

  public int defence(Building building);

  public void upgrade();
}
