public abstract class Animal implements Comparable<Animal> {

  String name;
  Integer age;

  public Animal(String animalName, Integer animalAge) {
    name = animalName;
    age = animalAge;
  }

  public Animal() {
    this("newborn", 0);
  }

  public String getName() {
    return name;
  }

  public Integer getAge() {
    return age;
  }

  public abstract void makeNoise();

  public abstract void eat(Food food) throws Exception;

  public abstract void eat(Food food, int repetitions) throws Exception;

  @Override
  public int compareTo(Animal a) {
    if (this.age == a.age) return 0;
    else if (this.age > a.age) return 1;
    else return -1;
  }
}
