package Services;

import DataManagers.CSVtoSQLReader;
import au.com.bytecode.opencsv.CSVReader;
import javafx.concurrent.Task;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.sql.*;
import java.util.logging.Logger;

/**
 * Class for a task that loads a CSV to a database table while reporting on the progress to the UI thread
 */
public final class CSVtoSQLReaderTask extends Task {

	private final Long totalCount;
	private final String path;
	private final String tableName;
	private final String campaignName;
	private final CSVtoSQLReader reader;

	/**
	 * @param databaseName Name of the database to work on
	 * @param totalCount Number of rows to be loaded in the table
	 * @param path Path to the file to be loaded
	 * @param tableName Name of the table to load the data to
	 */
	public CSVtoSQLReaderTask(String databaseName, Long totalCount, String path, String tableName, String campaignName) {
		this.totalCount = totalCount;
		this.path = path;
		this.tableName = tableName;
		this.campaignName = campaignName;
		this.reader = new CSVtoSQLReader(databaseName);
	}

	/**
	 * Method is a copy of the writeLogToTable method from CSVtoSQLReader but this one includes functionality
	 * to update the UI on its progress based on the number of lines processed so far and total row count of the file
	 *
	 * @param fileName Name of the file to read
	 * @param tableName Name of the table to load the data to
	 * @return
	 */
	public boolean writeLogToTableWithProgressUpdate(String fileName, String tableName) {
		try
		{
			File file = new File(fileName);
			CSVReader csvReader = new CSVReader(new InputStreamReader(new FileInputStream(file)));
			String columns = String.join(",", csvReader.readNext()).toLowerCase().replaceAll(" ", "_");

			//System.out.println(reader.setupInputTable(tableName, columns));

			if (!reader.setupInputTable(tableName, columns)) {
				reader.dropTable(tableName);
				throw new RuntimeException("Failed to setup input table");
			}

			int columnCount = columns.split(",").length;
			String query = reader.buildWriteQuery(tableName, columns, columnCount);

			Connection conn = DriverManager.getConnection(reader.getUrl());
			conn.setAutoCommit(false);
			PreparedStatement pstmt = conn.prepareStatement(query);

			int processedRows = 0;
			String[] nextLine;
			while ((nextLine = csvReader.readNext()) != null)
			{
				for (int i = 1; i <= columnCount; i++) {
					pstmt.setString(i, nextLine[i - 1]);
				}
				pstmt.addBatch();

				processedRows++;
				if (processedRows % 200000 == 0) {
					pstmt.executeBatch();
					updateProgress(processedRows/2, totalCount); // Updating UI with the progress
				}
			}

			pstmt.executeBatch();
			csvReader.close();
			conn.setAutoCommit(true);

			conn.close();
			return true;
		} catch (SQLException | IOException e)
		{
			//e.printStackTrace();
			throw new RuntimeException("Failed task");
		}
	}

	@Override
	protected Object call() throws SQLException {
		updateProgress(0, totalCount); // Starting the progress bar with 0 progress

		writeLogToTableWithProgressUpdate(path, tableName); // Running the task

		updateProgress(totalCount * 2/3,totalCount);

		if (tableName.contains("_click_log")) {
			reader.addFilterColumnsFromImpressionTable(tableName, campaignName + "_impression_log");
		}

		if (tableName.contains("_server_log")) {
			reader.addFilterColumnsFromImpressionTable(tableName, campaignName + "_impression_log");
		}

		updateProgress(totalCount,totalCount); // Finishing with a last update on completion
		return null;
	}
}
