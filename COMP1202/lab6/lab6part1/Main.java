public class Main {

  /**
   * Create a sample wolf, parrot, meat and plant object.
   *
   * @param args Standard terminal input.
   */
  public static void main(String[] args) {
    Wolf wolf = new Wolf("Wolfy", 5);
    Parrot parrot = new Parrot("Parroto", 2);

    Meat meat = new Meat("Bacon");
    Plant plant = new Plant("Avocado");
  }
}
