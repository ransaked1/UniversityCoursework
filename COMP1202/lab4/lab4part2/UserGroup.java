package lab4part2;

import java.util.ArrayList;

public class UserGroup {
  ArrayList<User> list = new ArrayList<User>();

  public ArrayList<User> getUsers() {
    return list;
  }

  public void addSampleData() {
    for (int i = 1; i <= 10; i++) {
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
}
