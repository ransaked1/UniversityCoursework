import static org.junit.jupiter.api.Assertions.*;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.PrintStream;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

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

@DisplayName("Lab 2 part 1")
class ATMTest {
	
	OutputCapturer outputHarness;
	
	@BeforeEach
	public void setup()
	{
		this.outputHarness = new OutputCapturer();
		this.outputHarness.start();
	}
	
	@AfterEach
	public void tearDown()
	{
		this.outputHarness.stop();
	}

	@Test
	@DisplayName("Test output of ATM")
	void testPart1() {
		
		Integer[] randomNumbers = { };
		String[] inputs = { new String("100") };
		Toolbox.setTestingData(randomNumbers, inputs);
		
		ATM atm = new ATM();
		atm.go();
		
		String out = outputHarness.getOutput();

		String op = "Welcome to online ATM banking\n" +
				"How much do you want in your account?\n" +
				"Enter your number";
		
		assertTrue(out.contains(op), "Output should be: Welcome to online ATM banking\n" +
				"How much do you want in your account?\n" +
				"Enter your number");
		assertTrue(out.contains("100"),"Testing that the balance is printed");
		
	}

}
