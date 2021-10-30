public abstract class Animal {

  String name;
  Integer age;

  public Animal(String animalName, Integer animalAge) {
    name = animalName;
    age = animalAge;
  }

  public Animal() {

  }

  public String getName() {
    return name;
  }

  public Integer getAge() {
    return age;
  }

  public abstract void makeNoise();

  public abstract void eat(Food food) throws Exception;
}

class Wolf extends Carnivore {
  public Wolf(String wolfName, Integer wolfAge) {
    super(wolfName, wolfAge);
  }

  @Override
  public void makeNoise() {
    System.out.println("wooof");
  }
}

class Parrot extends Omnivore {
  public Parrot(String parrotName, Integer parrotAge) {
    super(parrotName, parrotAge);
  }

  @Override
  public void makeNoise() {
    System.out.println("kraaa");
  }
}

class Giraffe extends Herbivore {
  public Giraffe(String giraffeName, Integer giraffeAge) {
    super(giraffeName, giraffeAge);
  }

  @Override
  public void makeNoise() {
    System.out.println("heee");
  }
}