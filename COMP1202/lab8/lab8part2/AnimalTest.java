import static org.junit.jupiter.api.Assertions.*;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;


import java.lang.reflect.Method;
import java.lang.reflect.Parameter;
import java.lang.reflect.Constructor;
import java.util.ArrayList;

class AnimalTest {

	class OutputCapturer {
		private PrintStream origOut;

		private ByteArrayOutputStream outputStream;

		public void start()
		{
			this.origOut = System.out;
			this.outputStream = new ByteArrayOutputStream();
			PrintStream ps = new PrintStream(this.outputStream);
			System.setOut(ps);
		}

		public String getOutput() {
			System.out.flush();
			return this.outputStream.toString().replaceAll("\\r\\n", "\n").replaceAll("\\r", "\n");
		}
		public void stop() {
			System.setOut(this.origOut);
		}
	}

	@Test
	@DisplayName("Test the eat method with just a Food parameter")
	void eatTest() {
		OutputCapturer outputHarness;

		outputHarness = new OutputCapturer();
		outputHarness.start();

		Wolf wolf1 = new Wolf("Tim", 2);
		Meat meat1 = new Meat("Venison");

		int no_eat = 1;
		try
		{
			wolf1.eat(meat1);
		}
		catch(Exception e)
		{
		}

		outputHarness.stop();
		int no_noise = outputHarness.getOutput().split("\n").length;

		assertEquals(no_eat, no_noise, "Testing that a new line is printed with an eating message");

	}

	@Test
	@DisplayName("Test the eat method with added Integer parameter")
	void eatTest2() {
		OutputCapturer outputHarness;

		outputHarness = new OutputCapturer();
		outputHarness.start();

		Wolf wolf1 = new Wolf("Tim", 2);
		Meat meat1 = new Meat("Venison");

		int no_eat = 5;
		try
		{
			wolf1.eat(meat1, 5);
		}
		catch(Exception e)
		{
		}

		outputHarness.stop();
		int no_noise = outputHarness.getOutput().split("\n").length;

		assertEquals(no_eat, no_noise, "Testing that a new line is printed for each eating message");

	}

	@Test
	@DisplayName("Test Parrot constructors and age methods")
	void parrotTest() {
		Integer pollyAge = 5;
		Integer francisAge = 10;
		Parrot polly = new Parrot(pollyAge);
		Parrot francis = new Parrot("Francis", francisAge);
		assertTrue(pollyAge == polly.getAge(), "Testing polly's age is correct");
		assertTrue(francisAge == francis.getAge(), "Testing francis's age is correct");
	}

	@Test
	@DisplayName("Test Wolf constructors and age methods")
	void animalTest() {
		Wolf newbornWolf = new Wolf();
		Integer age = 5;
		Wolf fuzzy = new Wolf("Fuzzy", age);

		assertTrue(0 == newbornWolf.getAge(), "Testing that the new born wolf's age is 0");
		assertTrue(age == fuzzy.getAge(), "Testing that fuzzy's age is correct");

	}

	@Test
	@DisplayName("Check if Wolf extends Carnivore")
	void wolfInheritanceTest(){
		System.out.println("The superclass of Wolf is " + Wolf.class.getSuperclass().getName());
		assertTrue(Wolf.class.getSuperclass().equals(Carnivore.class));
	}

	@Test
	@DisplayName("Check if Carnivore extends Animal")
	void carnivoreInheritanceTest(){
		System.out.println("The superclass of Carnivore is " + Carnivore.class.getSuperclass().getName());
		assertTrue(Carnivore.class.getSuperclass().equals(Animal.class));
	}
}