package Services;

import DataManagers.DataCalculateManager;
import javafx.concurrent.Task;

public class FetchDatesTask extends Task {
	private static final String INPUT_DB_NAME = "input_db";
	private final DataCalculateManager calcManager = new DataCalculateManager(INPUT_DB_NAME);
	private String tableName;

	public FetchDatesTask(String tableName) {
		this.tableName = tableName;
	}

	@Override
	protected Object call() throws Exception {
		calcManager.getFirstDateSingle(tableName);
		calcManager.getLastDateSingle(tableName);
		return null;
	}
}
