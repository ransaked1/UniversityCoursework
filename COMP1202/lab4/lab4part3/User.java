package lab4part2;

/**
 * User class with constructor that takes a username, name and user type.
 */
public class User {
  String username;
  String userType;
  String name;

  /**
   * User object constructor.
   *
   * @param username User's username.
   * @param userType User's type.
   * @param nm User's name.
   */
  public User(String username, String userType, String nm) {
    this.username = username;
    this.userType = userType;
    name = nm;
  }

  /**
   * Get user's username.
   *
   * @return Return the username.
   */
  public String getUsername() {
    return username;
  }

  /**
   * Get user's type.
   *
   * @return Return the type.
   */
  public String getUserType() {
    return userType;
  }

  /**
   * Set user's type.
   */
  public void setUserType(String type) {
    userType = type;
  }

  /**
   * Get user's name.
   *
   * @return Return the name.
   */
  public String getName() {
    return name;
  }
}
