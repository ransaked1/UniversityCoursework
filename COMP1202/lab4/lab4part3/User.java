package lab4part3;

public class User {
	String username;
	String userType;
	String name;

	public User(String uName, String uType, String nm) {
		username = uName;
		userType = uType;
		name = nm;
	}

	public String getUsername() {
		return username;
	}

	public String getUserType() {
		return userType;
	}

	public String getName() {
		return name;
	}

	public void setUserType(String type) {
		userType = type;
	}
}
