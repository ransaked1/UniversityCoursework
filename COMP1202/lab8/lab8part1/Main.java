public class Main {

  /**
   * Create a sample wolf, parrot, girrafe, meat and plant object. Test make noises methods for
   * animals. Test the eat method for herbivores and carnivores.
   *
   * @param args Standard terminal input.
   */
  public static void main(String[] args) {
    Wolf wolf = new Wolf();
    Parrot parrot = new Parrot(2);
    Giraffe giraffe = new Giraffe("Gira", 4);

    Meat meat = new Meat("Bacon");
    Plant plant = new Plant("Avocado");

    wolf.makeNoise();
    parrot.makeNoise();
    giraffe.makeNoise();

    try {
      giraffe.eat(plant);
      wolf.eat(meat, 3);
      parrot.eat(plant);
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}
