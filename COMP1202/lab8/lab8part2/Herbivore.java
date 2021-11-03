public abstract class Herbivore extends Animal{

	public Herbivore(String animalName, Integer animalAge) {
		name = animalName;
		age = animalAge;
	}

	@Override
	public void eat(Food food) throws Exception {
		if (food instanceof Plant) {
			System.out.println(name + " is eating a " + food.name);
		} else {
			throw new Exception(this.name + " can't eat a " + food.name);
		}
	}

	@Override
	public void eat(Food food, int repetitions) throws Exception {
		if (food instanceof Plant) {
			for (int i = 1; i <= repetitions; i++) {
				System.out.println(name + " is eating a " + food.name);
			}
		} else {
			throw new Exception(this.name + " can't eat a " + food.name);
		}
	}
}