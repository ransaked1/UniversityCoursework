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

class Wolf extends Animal {
  public Wolf(String wolfName, Integer wolfAge) {
    super(wolfName, wolfAge);
  }

  public void makeNoise() {
    System.out.println("wooof");
  }
}

class Parrot extends Animal {
  public Parrot(String parrotName, Integer parrotAge) {
    super(parrotName, parrotAge);
  }

  public void makeNoise() {
    System.out.println("kraaa");
  }
}