import java.util.ArrayList;
import java.util.Collections;

public class Demo {
	public static void main(String[] args) {
		ArrayList<Animal> animalList = new ArrayList<Animal>();

		animalList.add(new Wolf());
		animalList.add(new Parrot("Parroto", 4));
		animalList.add(new Wolf());
		animalList.add(new Giraffe("Gira", 2));
		animalList.add(new Wolf("Wolfy", 6));
		animalList.add(new Parrot("Parroty", 3));

		// What is an interface? An interface is an abstract class that imposes a certain structure when implemented.
		// How does it differ from an abstract class? All its methods are abstract and variables final static. Has no contructor.

		for (Animal a : animalList) {
			System.out.println("Name: " + a.name + " Age: " + a.age);
		}

		Collections.sort(animalList);
		System.out.println();

		for (Animal a : animalList) {
			System.out.println("Name: " + a.name + " Age: " + a.age);
		}

		// How could you make the animals be ordered from highest age to lowest age?
		// Swap the 1 and -1 returns in compareTo method.
	}
}