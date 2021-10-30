public abstract class Omnivore extends Animal{

	public Omnivore(String animalName, Integer animalAge) {
		name = animalName;
		age = animalAge;
	}

	@Override
	public void eat(Food food) {
		System.out.println(name + " is eating a " + food.name);
	}
}