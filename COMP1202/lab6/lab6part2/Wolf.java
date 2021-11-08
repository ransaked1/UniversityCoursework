class Wolf extends Animal {
  public Wolf(String wolfName, Integer wolfAge) {
    super(wolfName, wolfAge);
  }

  @Override
  public void makeNoise() {
    System.out.println("wooof");
  }
}
