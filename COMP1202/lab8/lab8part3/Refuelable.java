public abstract class Refuelable extends Transport {

  int maxFuelAmount;
  int fuelAmount;

  public Refuelable(String name, int maxFuelAmount, int fuelAmount) {
    super(name);
    this.maxFuelAmount = maxFuelAmount;
    this.fuelAmount = fuelAmount;
  }

  public Refuelable(String name, int maxFuelAmount) {
    super(name);
    this.maxFuelAmount = maxFuelAmount;
    this.fuelAmount = 0;
  }

  public int getFuelAmount() {
    return fuelAmount;
  }
}
