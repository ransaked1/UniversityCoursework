class Parrot extends Omnivore {
  public Parrot(String parrotName, Integer parrotAge) {
    super(parrotName, parrotAge);
  }

  @Override
  public void makeNoise() {
    System.out.println("kraaa");
  }
}
