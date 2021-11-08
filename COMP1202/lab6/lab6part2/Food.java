public abstract class Food {

  String name;

  public Food(String foodName) {
    name = foodName;
  }

  public String getName() {
    return name;
  }
}

class Meat extends Food {
  public Meat(String meatName) {
    super(meatName);
  }
}

class Plant extends Food {
  public Plant(String plantName) {
    super(plantName);
  }
}
