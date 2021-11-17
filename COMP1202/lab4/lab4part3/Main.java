package lab4part3;

import java.util.Iterator;

public class Main {

  /**
   * Creating a user group object and testing its functionality.
   *
   * @param args Standard terminal input.
   */
  public static void main(String[] args) {
    UserGroup userGroup = new UserGroup();
    UserGroup administrators = new UserGroup();
    userGroup.addSampleData();
    userGroup.printUsernames();

    Iterator<User> it = userGroup.getUserIterator();

    for (int i = 0; i < userGroup.list.size(); i++) {
      User currentUser = it.next();
      if (currentUser.userType == "admin") {
        administrators.list.add(currentUser);
      }
    }

    administrators.getUsers().get(userGroup.getUsers().size() - 1).setUserType("user");
    administrators.printUsernames();
  }
}
