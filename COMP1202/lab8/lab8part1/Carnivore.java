public abstract class Carnivore extends Animal {

  public Carnivore(String animalName, Integer animalAge) {
    name = animalName;
    age = animalAge;
  }

  public Carnivore() {
    this("newborn", 0);
  }

  @Override
  public void eat(Food food) throws Exception {
    if (food instanceof Meat) {
      System.out.println(name + " is eating a " + food.name);
    } else {
      throw new Exception(this.name + " can't eat a " + food.name);
    }
  }

  @Override
  public void eat(Food food, int repetitions) throws Exception {
    if (food instanceof Meat) {
      for (int i = 1; i <= repetitions; i++) {
        System.out.println(name + " is eating a " + food.name);
      }
    } else {
      throw new Exception(this.name + " can't eat a " + food.name);
    }
  }
}
