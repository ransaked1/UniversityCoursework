import static org.junit.jupiter.api.Assertions.*;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.util.jar.Attributes.Name;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

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
	@DisplayName("Test wolf makeNoise method")
	void wolflMakeNoiseTest() {

		OutputCapturer outputHarness;
		outputHarness = new OutputCapturer();
		outputHarness.start();

		Wolf wolf = new Wolf("Fuzzy", 8);
		wolf.makeNoise();

		outputHarness.stop();
		String w_noise = outputHarness.getOutput();

		assertTrue(w_noise.length()>0, "Testing that noise is printed");
	}

	@Test
	@DisplayName("Test parrot makeNoise method")
	void parrotMakeNoiseTest() {

		OutputCapturer outputHarness;
		outputHarness = new OutputCapturer();
		outputHarness.start();

		Parrot parrot = new Parrot("Squawker", 5);
		parrot.makeNoise();

		outputHarness.stop();
		String p_noise = outputHarness.getOutput();

		assertTrue(p_noise.length()>0, "Testing that parrot is printed");
	}

	@Test
	@DisplayName("Test make noise inhertience")
	void differentNoiseTest() {

		OutputCapturer outputHarness;
		outputHarness = new OutputCapturer();
		outputHarness.start();

		Wolf wolf = new Wolf("Fuzzy", 8);
		wolf.makeNoise();

		outputHarness.stop();
		String w_noise = outputHarness.getOutput();

		outputHarness = new OutputCapturer();
		outputHarness.start();

		Parrot parrot = new Parrot("Squawker", 5);
		parrot.makeNoise();

		outputHarness.stop();
		String p_noise = outputHarness.getOutput();

		assertFalse(p_noise.equals(w_noise), "Testing that a parraot and wolf make different noises");
	}
}