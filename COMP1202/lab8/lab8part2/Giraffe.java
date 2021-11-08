class Giraffe extends Herbivore {
  public Giraffe(String giraffeName, Integer giraffeAge) {
    super(giraffeName, giraffeAge);
  }

  @Override
  public void makeNoise() {
    System.out.println("heee");
  }
}
