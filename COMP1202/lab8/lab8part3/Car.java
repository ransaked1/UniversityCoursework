public class Car extends Refuelable implements RoadVehicle {

	public Car(String name, int maxFuelAmount, int fuelAmount) {
		super(name, maxFuelAmount, fuelAmount);
	}

	public void drift() {
	}

	public void start() {
	}

	public void stop() {
	}

	@Override
	public void makeSound() {
	}

	@Override
	public void setSpeed(int speed) {
	}
}