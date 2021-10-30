package lab4part3;

import java.util.ArrayList;
import java.util.Iterator;

public class UserGroup {
	ArrayList<User> list = new ArrayList<User>();

	public ArrayList<User> getUsers() {
		return list;
	}

	public void addSampleData() {
		for (int i = 1; i <= 10; i++){
			list.add(new User("dummy", "admin", "dummy"));
		}
	}

	public User getUser(int index) {
		return list.get(index);
	}

	public void printUsernames() {
		for (User user : list) {
			System.out.println(user.username + " " + user.userType);
		}
	}

	public void removeFirstUser() {
		list.remove(0);
	}

	public void removeLastUser() {
		list.remove(list.size() - 1);
	}

	public void removeUser(String username) {
		Iterator<User> it = list.iterator();

		for (int i = 0; i < list.size(); i++) {
			if (it.next().username == username) {
				it.remove();
			}
		}
	}

	public Iterator<User> getUserIterator() {
		Iterator<User> it = list.iterator();
		return it;
	}
}
