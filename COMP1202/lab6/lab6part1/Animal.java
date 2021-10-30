public abstract class Animal {

  String name;
  Integer age;

  public Animal(String animalName, Integer animalAge){
    name = animalName;
    age = animalAge;
  }

  public String getName() {
    return name;
  }

  public Integer getAge() {
    return age;
  }
}