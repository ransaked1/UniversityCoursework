package DataManagers;

import au.com.bytecode.opencsv.CSVReader;

import java.io.*;
import java.sql.*;
import java.util.ArrayList;

/**
 * Class manages reading the data from CSV to the SQL database
 */
public class CSVtoSQLReader {
	Connection conn;
	String url;

	/**
	 * @param databaseName Name of the database to work on
	 */
	public CSVtoSQLReader(String databaseName) {
		url = "jdbc:sqlite:" + databaseName + ".sqlite";
	}

	/**
	 * Check that a table exists in the database
	 *
	 * @param name Name of the table to check
	 * @return Boolean for the answer, true for present, false for not
	 * @throws SQLException Throw exception if failed to connect to database
	 */
	public boolean checkTableExists(String name) throws SQLException {
		String sql = "SELECT name "
				+ "FROM sqlite_master "
				+ "WHERE type='table' "
				+ "AND name='" + name + "'";

		conn = DriverManager.getConnection(url);
		PreparedStatement pstmt = conn.prepareStatement(sql);
		ResultSet rs = pstmt.executeQuery();

		int size =0;
		if (rs != null)
		{
			rs.next();
			size = rs.getRow(); // get row id
			if (size == 0)
				return false;
		}

		conn.close();
		return true;
	}

	/**
	 * Remove a table from the database
	 *
	 * @param name Name of the table to remove
	 * @throws SQLException Throw exception if failed to connect to database
	 */
	public void dropTable(String name) throws SQLException {
		String sql = "DROP TABLE IF EXISTS " + name;

		conn = DriverManager.getConnection(url);
		Statement stmt = conn.createStatement();
		stmt.execute(sql);
		conn.close();
	}

	/**
	 * Gets the values stored in a table column
	 *
	 * @param tableName Name of the table to take the contents from
	 * @param columnName Name of the column to get the data from
	 * @return An array list containing the data independent of its type
	 */
	public ArrayList<Object> getColumnContents(String tableName, String columnName) {
		ArrayList<Object> result = new ArrayList<>();
		String sql = "SELECT " + columnName + " FROM " + tableName;
		try {
			conn = DriverManager.getConnection(url);
			Statement stmt = conn.createStatement();
			ResultSet rs = stmt.executeQuery(sql);

			while (rs.next()) {
				result.add(rs.getString(columnName));
			}
		} catch (SQLException e) {
			throw new RuntimeException(e);
		}
		return result;
	}

	/**
	 * Count the number of row in the table
	 *
	 * @param name Name of the table to count
	 * @return Integer representing the column count
	 * @throws SQLException Throw exception if failed to connect to database
	 */
	public int tableRowCount(String name) throws SQLException {
		String sql = "SELECT COUNT(*) AS count FROM " + name;
		conn = DriverManager.getConnection(url);
		PreparedStatement pstmt = conn.prepareStatement(sql);
		ResultSet rs = pstmt.executeQuery(sql);
		conn.close();

		return rs.getInt("count");
	}

	/**
	 * Writing CSV data to the database
	 *
	 * @param fileName Name of the file to read the data from
	 * @param tableName Name of the database table to write data to
	 * @return True for success, false otherwise
	 */
	public boolean writeLogToTable(String fileName, String tableName) {
		try
		{
			File file = new File(fileName);
			CSVReader reader = new CSVReader(new InputStreamReader(new FileInputStream(file)));
			String columns = String.join(",", reader.readNext()).toLowerCase().replaceAll(" ", "_");

			// Setting up the table to hold the data
			setupInputTable(tableName, columns);

			int columnCount = columns.split(",").length;
			String query = buildWriteQuery(tableName, columns, columnCount);

			conn = DriverManager.getConnection(url);
			conn.setAutoCommit(false);
			PreparedStatement pstmt = conn.prepareStatement(query);

			// Going through the CSV line by line reading data
			int processedRows = 0;
			String[] nextLine;
			while ((nextLine = reader.readNext()) != null)
			{
				for (int i = 1; i <= columnCount; i++) {
					pstmt.setString(i, nextLine[i - 1]); // Writing data read to the query
				}
				pstmt.addBatch(); // Adding query to a batch for later execution

				// Count number of rows processed so far
				// Every 200000 rows execute the batched queries so far
				processedRows++;
				if (processedRows % 200000 == 0) {
					//System.out.println(processedRows);
					pstmt.executeBatch();
				}
			}

			pstmt.executeBatch();
			reader.close();
			conn.setAutoCommit(true);

			conn.close();
			return true;
		} catch (SQLException | IOException e) {
			//e.printStackTrace();
			return false;
		}
	}

	public void addFilterColumnsFromImpressionTable(String tableName, String impressionTableName) throws SQLException {
		//addFilterColumns(tableName);
		String pragma = "PRAGMA synchronous = OFF";
		String sql_update = buildUpdateQueryForColumn(tableName, impressionTableName);
		String drop = "DROP TABLE " + tableName;
		String rename = "ALTER TABLE temp RENAME TO " + tableName;

		conn = DriverManager.getConnection(url);
		Statement stmt = conn.createStatement();
		stmt.execute(pragma);
		stmt.execute(sql_update);
		stmt.execute(drop);
		stmt.execute(rename);
		conn.close();
	}

	private void addFilterColumns(String tableName) throws SQLException {
		String sql1 = "ALTER TABLE " + tableName + " ADD gender TEXT";
		String sql2 = "ALTER TABLE " + tableName + " ADD age TEXT";
		String sql3 = "ALTER TABLE " + tableName + " ADD income TEXT";
		String sql4 = "ALTER TABLE " + tableName + " ADD context TEXT";

		conn = DriverManager.getConnection(url);
		Statement stmt = conn.createStatement();
		stmt.execute(sql1);
		stmt.execute(sql2);
		stmt.execute(sql3);
		stmt.execute(sql4);
		conn.close();
	}

	private String buildUpdateQueryForColumn(String tableName, String impressionTableName) {
		String sql = "CREATE TABLE temp AS SELECT * FROM "
				+ "(SELECT DISTINCT * "
				+ "FROM " + tableName + " AS c "
				+ "LEFT JOIN (SELECT i.id AS impressionId, i.gender, i.age, i.income, i.context FROM " + impressionTableName
				+ " AS i WHERE impressionId IN (SELECT id FROM " + tableName + ")) AS i ON c.id = impressionId)";
		return sql;
	}

	/**
	 * Creates a table to hold all file data
	 *
	 * @param tableName Name of the table to fill with data
	 * @param columns Comma delimited string containing the column names for the table
	 * @throws SQLException Throw exception if failed to connect to database
	 */
	public boolean setupInputTable(String tableName, String columns) throws SQLException {
		if (columns.equals("")) {
			return false;
		}
		try {
			String sql = "CREATE TABLE IF NOT EXISTS " + tableName + " (item_id INTEGER PRIMARY KEY AUTOINCREMENT," + columns + ")";
			sql.replaceAll("\\bid\\b", "id TEXT");

			conn = DriverManager.getConnection(url);
			Statement stmt = conn.createStatement();
			stmt.execute(sql);
			conn.close();
			return true;
		}
		catch (Exception e) {
			return false;
		}
	}

	/**
	 * Building the query to fill with data later for the columns provided
	 *
	 * @param tableName Name of the table to build the query for
	 * @param columns Comma delimited string (no spaces) containing the column names for the table
	 * @param columnCount The number of columns present in the provided string
	 * @return Query with filters added
	 */
	public static String buildWriteQuery(String tableName, String columns, int columnCount) {
		String query = "INSERT INTO " + tableName + " (" + columns + ") VALUES (";
		for (int j = 1; j <= columnCount; j++) {
			query = query + "?,";
		}
		//Removing the last comma left from the loop above
		query = query.replaceAll(".$", "");
		query += ")";
		return query;
	}

	public String getUrl() {
		return url;
	}
}
