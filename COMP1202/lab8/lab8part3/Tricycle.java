public class Tricycle extends Transport implements Cycle {
  public Tricycle(String name) {
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
