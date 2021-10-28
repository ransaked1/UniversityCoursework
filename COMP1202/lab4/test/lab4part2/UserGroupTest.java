package lab4part2;

import static org.junit.jupiter.api.Assertions.*;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.util.ArrayList;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class UserGroupTest {

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
	@DisplayName("Test User and getter methods")
	void test() {
		User user = new User("aj", "user", "Alice Jones");
		assertEquals("aj", user.getUsername());
		assertEquals("user", user.getUserType());
		assertEquals("Alice Jones", user.getName());
	}
	
	@Test
	@DisplayName("Test the sample data")
	void testSampleData() {
		UserGroup users = new UserGroup();
		users.addSampleData();
		ArrayList<User> allUsers = users.getUsers();
		assertEquals(10, allUsers.size(), "Testing size of sample data is correct");
	}
	
	@Test
	void testGetUser() {
		UserGroup users = new UserGroup();
				
		String[] people = {"Kevin Rowe", "Jack Daniels", "Barry Smith", "Hugh Davies", "Pete Jackson", "Jerry Simpson", "Teresa Szelankovic", "Brian Degrasse Tyson", "Mike Hardcastle", "Danny Hanson"};
		for (int i=0; i<10; i++) {
		  users.getUsers().add(new User(Integer.toString(i), "user", people[i]));
		}
		
		assertEquals("Barry Smith", users.getUser(2).getName(), "Testing that the name is Barry Smith");
		
		OutputCapturer outputHarness = new OutputCapturer();
		outputHarness.start();
		
		users.printUsernames();
		
		outputHarness.stop();
		
		String output = outputHarness.getOutput();
		
		String expected = "0 user\n1 user\n2 user\n3 user\n4 user\n5 user\n6 user\n7 user\n8 user\n9 user\n";
				
		assertEquals(expected, output, "Testing printUsernane method");
		
		
	}

}
