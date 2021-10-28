package lab4part3;

import static org.junit.jupiter.api.Assertions.*;

import java.util.ArrayList;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;


class UserGroupTest {

	UserGroup users;
	
	@BeforeEach
	public void setup() {
		users = new UserGroup();
		
		users.getUsers().add(new User("id0", "user", "Kevin Rowe"));
		users.getUsers().add(new User("id1", "user", "Jack Daniels"));
		users.getUsers().add(new User("id2", "user", "Barry Smith"));
		users.getUsers().add(new User("id3", "user", "Hugh Davies"));
		users.getUsers().add(new User("id4", "user", "Pete Jackson"));
		users.getUsers().add(new User("id5", "user", "Jerry Simpson"));
		users.getUsers().add(new User("id6", "user", "Teresa Szelankovic"));
		users.getUsers().add(new User("id7", "user", "Brian Degrasse Tyson"));
		users.getUsers().add(new User("id8", "user", "Mike Hardcastle"));
		users.getUsers().add(new User("id9", "user", "Danny Hanson"));
	}
	
	@Test
	@DisplayName("Tests the removeFirstUser method")
	void testGetUser() {	
		User firstUser = users.getUser(0);
		users.removeFirstUser();
		assertFalse(users.getUsers().contains(firstUser), "Testing that the first user was removed");
		
	}
	
	@Test
	@DisplayName("Tests the removeLastUser method")
	void testGetUser2() {
		User lastUser = users.getUser(users.getUsers().size()-1);
		users.removeLastUser();
		assertFalse(users.getUsers().contains(lastUser), "Testing that the last user was removed");
		
	}

	@Test
	@DisplayName("Tests the removeUser method")
	void testGetUser3() {
		User newFirstUser = users.getUser(0);
		String newFirstUserName = users.getUser(0).getUsername();
		users.removeUser(newFirstUserName);
		
		assertFalse(users.getUsers().contains(newFirstUser), "Testing that the new first user was removed");
		
	}
	
}
