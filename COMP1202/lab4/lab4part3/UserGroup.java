package lab4part3;

import java.util.ArrayList;
import java.util.Iterator;

/**
 * UserGroup object that constructs, prints and removes from an array of User objects.
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

  /**
   * Remove the first user in the list.
   */
  public void removeFirstUser() {
    list.remove(0);
  }

  /**
   * Remove the last user in the list.
   */
  public void removeLastUser() {
    list.remove(list.size() - 1);
  }

  /**
   * Remove a user from the list based on their username.
   *
   * @param username The username of the user to remove.
   */
  public void removeUser(String username) {
    Iterator<User> it = list.iterator();
    
    for (int i = 0; i < list.size(); i++) {
      if (it.next().username == username) {
        it.remove();
      }
    }
  }

  /**
   * Return the user iterator.
   *
   * @return The iterator for that user object.
   */
  public Iterator<User> getUserIterator() {
    Iterator<User> it = list.iterator();
    return it;
  }
}
