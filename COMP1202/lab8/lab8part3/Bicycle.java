public class Bicycle extends Transport implements Cycle {
  public Bicycle(String name) {
    super(name);
  }

  public void drift() {}

  public void start() {}

  public void stop() {}

  public void liftFrontWheel() {}

  @Override
  public void makeSound() {}

  @Override
  public void setSpeed(int speed) {}
}
