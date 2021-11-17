public abstract class Animal implements Comparable<Animal> {

  String name;
  int age;

  public Animal() {
    this("newborn", 0);
  }

  public Animal(String animalName, int animalAge) {
    name = animalName;
    age = animalAge;
  }

  public String getName() {
    return name;
  }

  public int getAge() {
    return age;
  }

  public abstract void makeNoise();

  public abstract void eat(Food food) throws Exception;

  public abstract void eat(Food food, int repetitions) throws Exception;

  @Override
  public int compareTo(Animal a) {
    if (this.age == a.age) {
      return 0;
    } else if (this.age > a.age) {
      return 1;
    } else {
      return -1;
    }
  }
}
