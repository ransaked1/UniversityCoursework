package basic;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.ArrayList;

public class SimpleDB {
	final String connection_string = "jdbc:sqlite:comp3208-2015.db";
	public Connection c;
	ArrayList<int[]> ratings;

	public SimpleDB() {
		try {
			Class.forName("org.sqlite.JDBC");
			c = DriverManager.getConnection(connection_string);
			c.setAutoCommit(false);
			System.out.println("Opened database successfully");
		} catch (Exception e) {
			error(e);
		}
	}

	public void loadRatings() {
		try {
			System.out.println("Loading ratings");
			Statement stat = c.createStatement();
			ResultSet rs = stat.executeQuery("select * from newtraindata where rowid % 10 = 0");
			ratings = new ArrayList<>();
			while (rs.next()) {
				int[] rating = new int[3];
				rating[0] = rs.getInt(1);
				rating[1] = rs.getInt(2);
				rating[2] = rs.getInt(3);
				ratings.add(rating);
			}
			System.out.println("Done");
		} catch (Exception e) {
			error(e);
		}

	}

	public void createTable() {
		try {
			System.out.println("Creating Table");
			Statement stmt = c.createStatement();
			String sql = "CREATE TABLE temptraindata (UserID INT, ItemID INT, Rating INT)";
			stmt.executeUpdate(sql);
			c.commit();
			stmt.close();
			System.out.println("Done");
		} catch (Exception e) {
			error(e);
		}
	}

	public void addRatings() {
		try {
			System.out.println("Inserting Data");
			PreparedStatement stat = c.prepareStatement("INSERT INTO temptraindata VALUES (?,?,?)");
			for (int[] rating : ratings) {
				stat.setInt(1, rating[0]);
				stat.setInt(2, rating[1]);
				stat.setInt(3, rating[2]);
				stat.execute();			
			}
			c.commit();
			System.out.println("Done");
		} catch (Exception e) {
			error(e);
		}
	}

	public void error(Exception e) {
		System.err.println(e.getClass().getName() + ": " + e.getMessage());
		System.exit(0);
	}

	// TODO Auto-generated constructor stub
	public static void main(String[] args) {
		SimpleDB db = new SimpleDB();
		db.loadRatings();
		db.createTable();
		db.addRatings();

	}
}
