package DataManagers;

import java.sql.*;
import java.util.ArrayList;

/**
 * Class manages SQL operations for campaigns
 */
public class CampaignManager {
	Connection conn;
	String url;

	/**
	 * @param databaseName Name of the database to work on
	 */
	public CampaignManager(String databaseName) {
		url = "jdbc:sqlite:" + databaseName + ".sqlite";
	}

	/**
	 * Creates a table to hold all campaigns setup in the application
	 *
	 * @param tableName Name of the campaign table
	 * @param columns Comma delimited string containing the column names for the table
	 * @throws SQLException Throw exception if failed to connect to database
	 */
	public void setupCampaignTable(String tableName, String columns) throws SQLException {
		String sql = "CREATE TABLE IF NOT EXISTS " + tableName + " (" + columns + ")";

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

			//Reading data as strings, change getString into the expected type
			while (rs.next()) {
				result.add(rs.getString(columnName));
			}
		} catch (SQLException e) {
			throw new RuntimeException(e);
		}
		return result;
	}

	/**
	 * Writes a new campaign entry into the campaign table
	 *
	 * @param campaignName Name of the campaign to insert into the table
	 * @throws SQLException Throw exception if failed to connect to database
	 */
	public void addCampaign(String campaignName) throws SQLException {
		var campaignNameProcessed = campaignName.toLowerCase().replaceAll(" ", "_");
		String sql = "INSERT INTO campaigns (campaign_name, impression_table_name, click_table_name, server_table_name) VALUES "
				+ "('" + campaignName + "','" + campaignNameProcessed + "_impression_table','" + campaignNameProcessed
				+ "_click_table','" + campaignNameProcessed + "_server_table')";

		conn = DriverManager.getConnection(url);
		Statement stmt = conn.createStatement();
		stmt.execute(sql);
	}

	/**
	 * Removes a campaign from the campaign table
	 *
	 * @param campaignName Name of the campaign to drop from the table
	 * @throws SQLException Throw exception if failed to connect to database
	 */
	public void dropCampaign(String campaignName) throws SQLException {
		var campaignNameProcessed = campaignName.toLowerCase().replaceAll(" ", "_");
		String sql1 = "DELETE FROM campaigns WHERE campaign_name='" +  campaignName + "'";
		String sql2 = "DROP TABLE " + campaignNameProcessed + "_impression_log";
		String sql3 = "DROP TABLE " + campaignNameProcessed + "_click_log";
		String sql4 = "DROP TABLE " + campaignNameProcessed + "_server_log";

		conn = DriverManager.getConnection(url);
		Statement stmt = conn.createStatement();
		stmt.execute(sql1);
		stmt.execute(sql2);
		stmt.execute(sql3);
		stmt.execute(sql4);
	}
}
