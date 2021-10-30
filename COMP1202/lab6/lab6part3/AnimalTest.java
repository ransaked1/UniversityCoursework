import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class AnimalTest {

	@Test
	@DisplayName("Test for an exception when a carnivore eats a plant")
	void wolfEatsVegTest() {
		Wolf wolf = new Wolf("Fuzzy", 8);

		Assertions.assertThrows(Exception.class, () -> {
			wolf.eat(new Plant("Carrot"));
		});
	}

	@Test
	@DisplayName("Tests that there is no exception when a carnivore eats a meat")
	void wolfEatsMeatTest() {
		Wolf wolf = new Wolf("Fuzzy", 8);
		assertAll( () -> wolf.eat(new Meat("Chicken")) );
	}

}
