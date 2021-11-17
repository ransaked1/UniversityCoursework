package lab4part2;

import java.util.ArrayList;

/**
 * UserGroup object that constructs and prints an array of User objects.
 */
public class UserGroup {
  ArrayList<User> list = new ArrayList<User>();

  public ArrayList<User> getUsers() {
    return list;
  }

  /**
   * Function that adds sample data to a User object and adds it to the list.
   */
  public void addSampleData() {
    for (int i = 1; i <= 10; i++) {
      list.add(new User("dummy", "admin", "dummy"));
    }
  }

  /**
   * Get a user object at that index.
   *
   * @param index Index of the user in the list.
   * @return The user object.
   */
  public User getUser(int index) {
    return list.get(index);
  }

  /**
   * Printing all the user object's data.
   */
  public void printUsernames() {
    for (User user : list) {
      System.out.println(user.username + " " + user.userType);
    }
  }
}
