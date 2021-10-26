import static org.junit.jupiter.api.Assertions.*;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.PrintStream;
import java.lang.SecurityManager;
import java.lang.reflect.ReflectPermission;

import org.junit.Rule;
import org.junit.rules.Timeout;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import java.security.Permission;
import java.util.PropertyPermission;
import java.lang.RuntimePermission;
import java.util.logging.LoggingPermission;

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

@DisplayName("Lab 2 part 2")
class ATMTest {
	
	OutputCapturer outputHarness;
	SecurityManager oldSM;

	@BeforeEach
	public void setupTest() {
		oldSM = System.getSecurityManager();
		SecurityManager sm = new SecurityManager() {
			@Override
			public void checkPermission(Permission perm) {
			}
			@Override
			public void checkPermission(Permission perm, Object count) {	
			}
			@Override
			public void checkExit(int status) {
				throw new SecurityException("System exit called");
			}
		};	
		System.setSecurityManager(sm);
		
		this.outputHarness = new OutputCapturer();
		this.outputHarness.start();
	}
	
	@AfterEach
	public void tearDown()
	{
		this.outputHarness.stop();
		System.setSecurityManager(oldSM);
	}
	
	@Test
	@DisplayName("Test ATM's inquire method")
	void testPart2Inquire() {
		
		Integer[] randomNumbers = { };
		String[] inputs = { new String("100"), new String("3")};
		Toolbox.setTestingData(randomNumbers, inputs);
		
		ATM atm = new ATM();
		atm.go();
		
		String out = outputHarness.getOutput();
				
		assertTrue(out.contains("100"), "correct balance is shown");
	}

	@Test
	@DisplayName("Test ATM's deposit method")
	void testPart2Deposit() {
		
		Integer[] randomNumbers = { };
		String[] inputs = { new String("100"), new String("2"), new String ("20")};
		Toolbox.setTestingData(randomNumbers, inputs);
		
		ATM atm = new ATM();
		atm.go();
		
		String out = outputHarness.getOutput();
		assertTrue(out.contains("120"), "correct balance is shown");
	}
	
	@Test
	@DisplayName("Test ATM's withdraw method")
	void testPart2Withdraw() {
		
		Integer[] randomNumbers = { };
		String[] inputs = { new String("100"), new String("1"), new String ("20")};
		Toolbox.setTestingData(randomNumbers, inputs);
		
		ATM atm = new ATM();
		atm.go();
		
		String out = outputHarness.getOutput();
		assertTrue(out.contains("80"), "correct balance is shown");
	
	}
}
