public class Main {

  /**
   * Create a sample wolf, parrot, girrafe, meat and plant object. Test make noises methods for
   * animals.
   *
   * @param args Standard terminal input.
   */
  public static void main(String[] args) {
    Wolf wolf = new Wolf("Wolfy", 5);
    Parrot parrot = new Parrot("Parroto", 2);
    Girrafe girrafe = new Girrafe("Girra", 4);

    Meat meat = new Meat("Bacon");
    Plant plant = new Plant("Avocado");

    wolf.makeNoise();
    parrot.makeNoise();
  }
}
