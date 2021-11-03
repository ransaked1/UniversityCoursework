public abstract class Transport {

	String name;
	int speed = 0;

	public Transport(String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}

	public int getSpeed() {
		return speed;
	}

	public abstract void makeSound();
	public abstract void setSpeed(int speed);
}