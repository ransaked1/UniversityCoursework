package lab4part2;

public class Main {

  /**
   * Creating a user group object and testing its functionality.
   *
   * @param args Standard terminal input.
   */
  public static void main(String[] args) {
    UserGroup userGroup = new UserGroup();
    userGroup.addSampleData();
    userGroup.printUsernames();
  }
}
