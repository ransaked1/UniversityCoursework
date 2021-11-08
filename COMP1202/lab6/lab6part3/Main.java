public class Main {
  public static void main(String[] args) {
    Wolf wolf = new Wolf("Wolfy", 5);
    Parrot parrot = new Parrot("Parroto", 2);
    Giraffe giraffe = new Giraffe("Gira", 4);

    Meat meat = new Meat("Bacon");
    Plant plant = new Plant("Avocado");

    wolf.makeNoise();
    parrot.makeNoise();
    giraffe.makeNoise();

    try {
      giraffe.eat(plant);
      wolf.eat(plant);
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}
