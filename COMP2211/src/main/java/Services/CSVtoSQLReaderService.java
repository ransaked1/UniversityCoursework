package Services;

import javafx.concurrent.Service;
import javafx.concurrent.Task;

/**
 * Wrapper service for the CSVtoSQLReader task
 */
public final class CSVtoSQLReaderService extends Service {

	Long count;
	String path;
	String table;
	String databaseName;
	String campaignName;
	Task serviceTask = null;

	public CSVtoSQLReaderService(String databaseName, Long count, String path, String table, String campaignName) {
		this.count = count;
		this.databaseName = databaseName;
		this.path = path;
		this.table = table;
		this.campaignName = campaignName;
	}

	/**
	 * Starting a task with the values provided
	 * @return Task to be run by service
	 */
	@SuppressWarnings("rawtypes")
	@Override
	protected Task createTask() {
		serviceTask = new CSVtoSQLReaderTask(databaseName, count, path, table, campaignName);
		return serviceTask;
	}

	public void cancelTask() {
		if (serviceTask != null)
			serviceTask.cancel(true);
	}
}
