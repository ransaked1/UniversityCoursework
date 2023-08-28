package Services;

import DataManagers.DataCalculateManager;
import javafx.concurrent.Task;

/**
 * Class sets up a task to run the correct calculation manager method
 */
public final class MetricCalculatorTask extends Task {
	// Constants for the database to use and operation options used when threading the fetching and calculations
	private static final String INPUT_DB_NAME = "input_db";
	private static final int FETCH_IMPRESSION = 0;
	private static final int FETCH_CLICKS = 1;
	private static final int FETCH_UNIQUES = 2;
	private static final int FETCH_BOUNCES = 3;
	private static final int FETCH_CONVERSIONS = 4;
	private static final int FETCH_COST = 5;
	private static final int CALCULATE_CTR = 6;
	private static final int CALCULATE_CPM = 7;
	private static final int CALCULATE_CPC = 8;
	private static final int CALCULATE_CPA = 9;
	private static final int CALCULATE_BOUNCE_RATE = 10;

	String impressionTableName;
	String clickTableName;
	String serverTableName;
	int operation;

	private final DataCalculateManager calcManager = new DataCalculateManager(INPUT_DB_NAME);

	/**
	 * @param operation Operation code between 0 and 10
	 * @param impressionTableName
	 * @param clickTableName
	 * @param serverTableName
	 */
	public MetricCalculatorTask(int operation, String impressionTableName, String clickTableName, String serverTableName) {
		this.operation = operation;
		this.impressionTableName = impressionTableName;
		this.clickTableName = clickTableName;
		this.serverTableName = serverTableName;
	}

	/**
	 * Switch clause picks the appropriate method based on the code provided
	 * @return
	 */
	@Override
	protected Object call() {
		switch (operation) {
			case FETCH_IMPRESSION -> calcManager.fetchImpressions(impressionTableName);
			case FETCH_CLICKS -> calcManager.fetchClicks(clickTableName);
			case FETCH_UNIQUES -> calcManager.fetchUniques(clickTableName);
			case FETCH_BOUNCES -> calcManager.fetchBounces(serverTableName);
			case FETCH_CONVERSIONS -> calcManager.fetchConversions(serverTableName);
			case FETCH_COST -> calcManager.fetchCost(clickTableName, impressionTableName);
			case CALCULATE_CTR -> calcManager.calculateCTR(clickTableName, impressionTableName);
			case CALCULATE_CPM -> calcManager.calculateCPM(clickTableName, impressionTableName);
			case CALCULATE_CPC -> calcManager.calculateCPC(clickTableName);
			case CALCULATE_CPA -> calcManager.calculateCPA(clickTableName, impressionTableName, serverTableName);
			case CALCULATE_BOUNCE_RATE -> calcManager.calculateBounceRate(clickTableName, serverTableName);
		}
		return null;
	}
}
