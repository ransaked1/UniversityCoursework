public abstract class Animal {

  String name;
  int age;

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
}
