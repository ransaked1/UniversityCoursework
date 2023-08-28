package DataManagers;

import java.sql.*;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Objects;

/**
 * Class manages the metrics that only need fetching from the database
 */
public class DataFetchManager {
	Connection conn;
	String url;

	private static final int GENDER_FEMALE = 1;
	private static final int GENDER_MALE = 2;

	private static final int AGE_UNDER_25 = 0;
	private static final int AGE_25_TO_34 = 1;
	private static final int AGE_35_TO_44 = 2;
	private static final int AGE_45_TO_54 = 3;
	private static final int AGE_ABOVE_54 = 4;

	private static final int INCOME_LOW = 0;
	private static final int INCOME_MEDIUM = 1;
	private static final int INCOME_HIGH = 2;

	private static final int CONTEXT_SM = 0;
	private static final int CONTEXT_SHOPPING = 1;
	private static final int CONTEXT_BLOG = 2;
	private static final int CONTEXT_NEWS = 3;

	private static String timeFilterStart = "";
	private static String timeFilterEnd = "";
	private static Integer impressions = 0;
	private static Integer clicks = 0;
	private static Integer uniques = 0;
	private static Integer bounces = 0;
	private static Integer conversions = 0;
	private static Double cost = 0.0;
	private static Double clicks_cost = 0.0;
	private static String start = "";
	private static String end = "";
	private static boolean[] ages = new boolean[] {true,true,true,true,true};
	private static boolean[] incomes = new boolean[] {true,true,true};
	private static boolean[] contexts = new boolean[] {true,true,true,true};

	private static int genderFilter = 0;
	public static int bounceFilter = 0;

	public DataFetchManager(String databaseName) {
		url = "jdbc:sqlite:" + databaseName +".sqlite";
	}

	public void getFirstDateSingle(String tableName) throws SQLException {
		String sql = "SELECT * FROM " + tableName + " AS date ORDER BY date ASC LIMIT 1";

		conn = DriverManager.getConnection(url);
		Statement stmt = conn.createStatement();
		ResultSet rs = stmt.executeQuery(sql);
		String s = rs.getString("date");
		conn.close();
		start = s;
	}

	public void getLastDateSingle(String tableName) throws SQLException {
		String sql = "SELECT * FROM " + tableName + " AS date ORDER BY date DESC LIMIT 1";

		conn = DriverManager.getConnection(url);
		Statement stmt = conn.createStatement();
		ResultSet rs = stmt.executeQuery(sql);
		String s = rs.getString("date");
		conn.close();
		end = s;
	}

	/**
	 * Fetching the impression count, has to be an impression table
	 *
	 * @param tableName Name of table to count from
	 * @return True on success, false on failure
	 */
	public boolean fetchImpressions(String tableName) {
		try {
			String sql = "SELECT COUNT(*) AS count FROM " + tableName + " WHERE id IS NOT NULL ";

			sql = addTimeFiltersDateColumn(sql);
			sql = addFilters(sql);

			conn = DriverManager.getConnection(url);
			Statement stmt = conn.createStatement();
			ResultSet rs = stmt.executeQuery(sql);
			impressions = rs.getInt("count");
			conn.close();
			return true;
		} catch (SQLException e) {
			return false;
		}
	}

	/**
	 * Fetching the clicks count, has to be a clicks table
	 *
	 * @param tableName Name of table to count from
	 * @return True on success, false on failure
	 */
	public boolean fetchClicks(String tableName) {
		try {
			String sql = "SELECT COUNT(*) AS count FROM " + tableName + " WHERE id IS NOT NULL";

			sql = addTimeFiltersDateColumn(sql);
			sql = addFilters(sql);

			conn = DriverManager.getConnection(url);
			Statement stmt = conn.createStatement();
			ResultSet rs = stmt.executeQuery(sql);
			clicks = rs.getInt("count");
			conn.close();
			return true;
		} catch (SQLException e) {
			return false;
		}
	}


	/**
	 * Fetching the unique user ids count, has to be a clicks table
	 *
	 * @param tableName Name of table to count from
	 * @return True on success, false on failure
	 */
	public boolean fetchUniques(String tableName) {
		try {
			String sql = "SELECT COUNT(DISTINCT id) AS count FROM " + tableName + " WHERE 'id:1' IS NOT NULL ";

			sql = addTimeFiltersDateColumn(sql);
			sql = addFilters(sql);

			conn = DriverManager.getConnection(url);
			Statement stmt = conn.createStatement();
			ResultSet rs = stmt.executeQuery(sql);
			uniques = rs.getInt("count");
			conn.close();
			return true;
		} catch (SQLException e) {
			return false;
		}
	}

	/**
	 * Method applies time filters specified to a query with only one date column
	 *
	 * @param sql Query to apply the filter to
	 * @return Resulting sql query with filters applied
	 */
	String addTimeFiltersDateColumn(String sql) {
		// Options for only end date specified, only start date specified and both dates specified
		if (Objects.equals(timeFilterStart, "") && !Objects.equals(timeFilterEnd, "")) {
			sql += " AND date <= '" + timeFilterEnd + "'";
		} else if (!Objects.equals(timeFilterStart, "") && Objects.equals(timeFilterEnd, "")) {
			sql += " AND date >= '" + timeFilterStart + "'";
		} else if (!Objects.equals(timeFilterStart, "") && !Objects.equals(timeFilterEnd, "")) {
			sql += " AND date BETWEEN '" + timeFilterStart + "' AND '" + timeFilterEnd + "'";
		}
		return sql;
	}

	/**
	 * Fetching the bounce count, has to be a server table
	 *
	 * @param tableName Name of table to count from
	 * @return True on success, false on failure
	 */
	public boolean fetchBounces(String tableName) {
		if (bounceFilter == 0){
			try {
				String sql = "SELECT COUNT(*) AS count  FROM " + tableName + " WHERE  ((julianday(exit_date) - julianday(entry_date)) * 86400.0) < 30";

				sql = addTimeFiltersDoubleDateColumn(sql);
				sql = addFilters(sql);

				conn = DriverManager.getConnection(url);
				Statement stmt = conn.createStatement();
				ResultSet rs = stmt.executeQuery(sql);
				bounces = rs.getInt("count");
				conn.close();

				return true;
			} catch (SQLException e) {
				return false;
			}
		}
		if (bounceFilter == 1) {
			try {
				String sql = "SELECT COUNT(*) AS count FROM " + tableName + " WHERE pages_viewed = '1'";

				sql = addTimeFiltersDoubleDateColumn(sql);
				sql = addFilters(sql);

				conn = DriverManager.getConnection(url);
				Statement stmt = conn.createStatement();
				ResultSet rs = stmt.executeQuery(sql);
				bounces = rs.getInt("count");
				conn.close();
				return true;
			} catch (SQLException e) {
				return false;
			}
		}
		return false;
	}

	/**
	 * Fetching the conversion count
	 *
	 * @param tableName Name of table to count from, has to be a server table
	 * @return True on success, false on failure
	 */
	public boolean fetchConversions(String tableName) {
		try {
			String sql = "SELECT COUNT(*) AS count FROM " + tableName + " WHERE conversion = 'Yes'";

			sql = addTimeFiltersDoubleDateColumn(sql);
			sql = addFilters(sql);

			conn = DriverManager.getConnection(url);
			Statement stmt = conn.createStatement();
			ResultSet rs = stmt.executeQuery(sql);
			conversions = rs.getInt("count");
			conn.close();
			return true;
		} catch (SQLException e) {
			return false;
		}
	}

	/**
	 * Method applies time filters specified to a query with two date columns
	 *
	 * @param sql Query to apply the filter to
	 * @return True on success, false on failure
	 */
	String addTimeFiltersDoubleDateColumn(String sql) {
		// Options for only end date specified, only start date specified and both dates specified
		if (Objects.equals(timeFilterStart, "") && !Objects.equals(timeFilterEnd, "")) {
			sql += " AND exit_date <= '" + timeFilterEnd + "'";
		} else if (!Objects.equals(timeFilterStart, "") && Objects.equals(timeFilterEnd, "")) {
			sql += " AND entry_date >= '" + timeFilterStart + "'";
		} else if (!Objects.equals(timeFilterStart, "") && !Objects.equals(timeFilterEnd, "")) {
			sql += " AND entry_date >= '" + timeFilterStart + "' AND exit_date <= '" + timeFilterEnd + "'";
		}
		return sql;
	}

	/**
	 * Method builds a query to add up impression and click costs to a total
	 *
	 * @param clickTableName Name of click table to count from
	 * @param impressionTableName Name of impression table to count from
	 * @return True on success, false on failure
	 */
	public boolean fetchCost(String clickTableName, String impressionTableName) {
		try {
			String sql = "SELECT SUM(cost) cost\n"
					+ "FROM\n"
					+ " (SELECT SUM(click_cost) AS cost FROM " + clickTableName + " WHERE click_cost IS NOT NULL ";

			sql = addTimeFiltersDateColumn(sql);
			sql = addFilters(sql);

			sql += " UNION ALL SELECT SUM(impression_cost) AS cost FROM " + impressionTableName + " WHERE impression_cost IS NOT NULL ";

			sql = addTimeFiltersDateColumn(sql);
			sql = addFilters(sql);

			sql += ")";

			conn = DriverManager.getConnection(url);
			Statement stmt = conn.createStatement();
			ResultSet rs = stmt.executeQuery(sql);
			cost = rs.getDouble("cost");
			cost = Math.floor(cost * 100) / 100;
			conn.close();
			return true;
		} catch (SQLException e) {
			return false;
		}
	}



	 protected boolean fetchClicksCost(String clickTableName) {
		try {
			String sql = "SELECT SUM(click_cost) AS cost FROM " + clickTableName + " WHERE click_cost IS NOT NULL ";

			sql = addTimeFiltersDateColumn(sql);
			sql = addFilters(sql);

			conn = DriverManager.getConnection(url);
			Statement stmt = conn.createStatement();
			ResultSet rs = stmt.executeQuery(sql);
			clicks_cost = rs.getDouble("cost");
			clicks_cost = Math.floor(clicks_cost * 100) / 100;
			conn.close();
			return true;
		} catch (SQLException e) {
			return false;
		}
	}

	/**
	 * Execute all the fetch operation available with the three data tables provided
	 *
	 * @param impressionTableName
	 * @param clickTableName
	 * @param serverTableName
	 */
	public void fetchAll(String impressionTableName, String clickTableName, String serverTableName) {
		fetchImpressions(impressionTableName);
		fetchClicks(clickTableName);
		fetchUniques(clickTableName);
		fetchBounces(serverTableName);
		fetchConversions(serverTableName);
		fetchCost(clickTableName,impressionTableName);
	}

	/**
	 * Method resets filters to empty values if set
	 */
	public void resetFilters() {
		setTimeFilterStart("");
		setTimeFilterEnd("");
		ages = new boolean[] {true,true,true,true,true};
		incomes = new boolean[] {true,true,true};
		contexts = new boolean[] {true,true,true,true};
		setBounceFilter(0);
		setGenderFilter(0);
	}

	String addFilters(String sql) {
		sql = addGenderFilter(sql);
		sql = addAgeFilters(sql);
		sql = addIncomeFilters(sql);
		sql = addContextFilters(sql);
		//System.out.println(sql);
		return sql;
	}

	private String addAgeFilters(String sql) {
		sql += " AND (age IS NULL";

		for (int i = 0; i < ages.length; i++) {
			if (ages[i]) {
				sql += " OR ";

				switch (i) {
					case AGE_UNDER_25 -> sql += "age='<25'";
					case AGE_25_TO_34 -> sql += "age='25-34'";
					case AGE_35_TO_44 -> sql += "age='35-44'";
					case AGE_45_TO_54 -> sql += "age='45-54'";
					case AGE_ABOVE_54 -> sql += "age='>54'";
				}
			}
		}
		sql += ")";
		return sql;
	}

	private String addIncomeFilters(String sql) {
		sql += " AND (income IS NULL";

		for (int i = 0; i < incomes.length; i++) {
			if (incomes[i]) {
				sql += " OR ";

				switch (i) {
					case INCOME_LOW -> sql += "income='Low'";
					case INCOME_MEDIUM -> sql += "income='Medium'";
					case INCOME_HIGH -> sql += "income='High'";
				}
			}
		}
		sql += ")";
		return sql;
	}

	private String addContextFilters(String sql) {
		sql += " AND (context IS NULL";

		for (int i = 0; i < contexts.length; i++) {
			if (contexts[i]) {
				sql += " OR ";

				switch (i) {
					case CONTEXT_SM -> sql += "context='Social Media'";
					case CONTEXT_SHOPPING -> sql += "context='Shopping'";
					case CONTEXT_BLOG -> sql += "context='Blog'";
					case CONTEXT_NEWS -> sql += "context='News'";
				}
			}
		}
		sql += ")";
		return sql;
	}

	private String addGenderFilter(String sql) {
		if (genderFilter == GENDER_FEMALE) {
			sql += " AND gender = 'Female'";
		}
		if (genderFilter == GENDER_MALE) {
			sql += " AND gender = 'Male'";
		}
		return sql;
	}

	public String reverseDateFormat(String input) throws ParseException {
		SimpleDateFormat in = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
		Date dateValue = in.parse(input);
		SimpleDateFormat output = new SimpleDateFormat("dd-MM-yyyy HH:mm:ss");
		return output.format(dateValue);
	}

	public String getTimeFilterStart() {
		return timeFilterStart;
	}
	public Integer getImpressions() {
		return impressions;
	}
	public void setTimeFilterStart(String timeFilterStart) {
		this.timeFilterStart = timeFilterStart;
	}
	public String getTimeFilterEnd() {
		return timeFilterEnd;
	}
	public void setTimeFilterEnd(String timeFilterEnd) {
		this.timeFilterEnd = timeFilterEnd;
	}
	public Integer getClicks() {
		return clicks;
	}
	public  Integer getUniques() {
		return uniques;
	}
	public Integer getBounces() {
		return bounces;
	}
	public Integer getConversions() {
		return conversions;
	}
	public Double getCost() {
		return cost;
	}
	public String getStart() {
		return start;
	}
	public String getEnd() {
		return end;
	}
	public static void setGenderFilter(int genderFilter) {
		DataFetchManager.genderFilter = genderFilter;
	}

	public boolean getAgeFilter(int index) {
		return ages[index];
	}

	public void setAgeFilter(int index, boolean value) {
		ages[index] = value;
	}

	public boolean getIncomeFilter(int index) {
		return incomes[index];
	}

	public void setIncomeFilter(int index, boolean value) {
		incomes[index] = value;
	}

	public boolean getContextFilter(int index) {
		return contexts[index];
	}

	public void setContextFilter(int index, boolean value) {
		contexts[index] = value;
	}

	public void setBounceFilter(int bounceFilter) {DataFetchManager.bounceFilter = bounceFilter;}

	protected Double getClicksCost() {
		return clicks_cost;
	}

}
