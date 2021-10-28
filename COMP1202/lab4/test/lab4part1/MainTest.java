package lab4part1;

import static org.junit.jupiter.api.Assertions.*;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

class MainTest {

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

	OutputCapturer outputHarness;

	@BeforeEach
	public void setup() {
		this.outputHarness = new OutputCapturer();
		this.outputHarness.start();
	}
	
	@AfterEach
	public void tearDown() {
		this.outputHarness.stop();
	}

	@Test
	@DisplayName("Test times table output")
	void testTimesTable() {
		Integer[] randoms = {};
		String[] inputs = { new String("319") };	
		Toolbox.setTestingData(randoms, inputs);
		
		Main.main(null);
		
		String out = outputHarness.getOutput();
		
		assertTrue(out.contains("5742"), "319 times 18");
		assertTrue(out.contains("6061"), "319 times 19");
		assertTrue(out.contains("6380"), "319 times 20");

	}
	@Test
	@DisplayName("Test sum calculation")
	void testSum() {
		Integer[] randoms = {};
		String[] inputs = { new String("0") };
		Toolbox.setTestingData(randoms, inputs);
		
		Main.main(null);
		
		String out = outputHarness.getOutput();
		Pattern p = Pattern.compile("(?<!\\d)32(?!\\d)");
		Matcher m = p.matcher(out);
		assertTrue(m.find(), "provided the right answer");
	}

}
