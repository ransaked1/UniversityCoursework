public abstract class Animal {

  String name;
  int age;

  public Animal(String animalName, int animalAge) {
    name = animalName;
    age = animalAge;
  }

  public Animal() {}

  public String getName() {
    return name;
  }

  public int getAge() {
    return age;
  }

  public abstract void makeNoise();

  public abstract void eat(Food food) throws Exception;
}
