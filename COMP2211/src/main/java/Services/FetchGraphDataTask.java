package Services;

import DataManagers.DataGraphManager;
import javafx.concurrent.Task;

public class FetchGraphDataTask extends Task {
	private static final String INPUT_DB_NAME = "input_db";
	private static final int GRANULARITY_HOUR = 0;
	private static final int GRANULARITY_DAY = 1;
	private static final int GRANULARITY_WEEK = 2;

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
	private static final int CALCULATE_DISTRIBUTION = 11;

	String impressionTableName;
	String clickTableName;
	String serverTableName;
	int operation;
	int granularity;
	private final DataGraphManager graphManager = new DataGraphManager(INPUT_DB_NAME);

	public FetchGraphDataTask(int operation, String impressionTableName, String clickTableName, String serverTableName) {
		this.operation = operation;
		this.impressionTableName = impressionTableName;
		this.clickTableName = clickTableName;
		this.serverTableName = serverTableName;
	}

	@Override
	protected Object call() throws Exception {
		switch (operation) {
			case FETCH_IMPRESSION -> {
				graphManager.fetchImpressionsPoints(impressionTableName, GRANULARITY_HOUR);
				graphManager.fetchImpressionsPoints(impressionTableName, GRANULARITY_DAY);
				graphManager.fetchImpressionsPoints(impressionTableName, GRANULARITY_WEEK);
			}
			case FETCH_CLICKS -> {
				graphManager.fetchClicksPoints(clickTableName, GRANULARITY_HOUR);
				graphManager.fetchClicksPoints(clickTableName, GRANULARITY_DAY);
				graphManager.fetchClicksPoints(clickTableName, GRANULARITY_WEEK);
			}
			case FETCH_UNIQUES -> {
				graphManager.fetchUniquesPoints(clickTableName, GRANULARITY_HOUR);
				graphManager.fetchUniquesPoints(clickTableName, GRANULARITY_DAY);
				graphManager.fetchUniquesPoints(clickTableName, GRANULARITY_WEEK);
			}
			case FETCH_BOUNCES -> {
				graphManager.fetchBouncesPoints(serverTableName, GRANULARITY_HOUR);
				graphManager.fetchBouncesPoints(serverTableName, GRANULARITY_DAY);
				graphManager.fetchBouncesPoints(serverTableName, GRANULARITY_WEEK);
			}
			case FETCH_CONVERSIONS -> {
				graphManager.fetchConversionsPoints(serverTableName, GRANULARITY_HOUR);
				graphManager.fetchConversionsPoints(serverTableName, GRANULARITY_DAY);
				graphManager.fetchConversionsPoints(serverTableName, GRANULARITY_WEEK);
			}
			case FETCH_COST -> {
				graphManager.fetchCostsPoints(clickTableName, impressionTableName, GRANULARITY_HOUR);
				graphManager.fetchCostsPoints(clickTableName, impressionTableName, GRANULARITY_DAY);
				graphManager.fetchCostsPoints(clickTableName, impressionTableName, GRANULARITY_WEEK);
			}
			case CALCULATE_CTR -> {
				graphManager.calculateCtrPoints(clickTableName, impressionTableName, GRANULARITY_HOUR);
				graphManager.calculateCtrPoints(clickTableName, impressionTableName, GRANULARITY_DAY);
				graphManager.calculateCtrPoints(clickTableName, impressionTableName, GRANULARITY_WEEK);
			}
			case CALCULATE_CPM -> {
				graphManager.calculateCpmPoints(clickTableName, impressionTableName, GRANULARITY_HOUR);
				graphManager.calculateCpmPoints(clickTableName, impressionTableName, GRANULARITY_DAY);
				graphManager.calculateCpmPoints(clickTableName, impressionTableName, GRANULARITY_WEEK);
			}
			case CALCULATE_CPC -> {
				graphManager.calculateCpcPoints(clickTableName, impressionTableName, GRANULARITY_HOUR);
				graphManager.calculateCpcPoints(clickTableName, impressionTableName, GRANULARITY_DAY);
				graphManager.calculateCpcPoints(clickTableName, impressionTableName, GRANULARITY_WEEK);
			}
			case CALCULATE_CPA -> {
				graphManager.calculateCpaPoints(clickTableName, impressionTableName, serverTableName, GRANULARITY_HOUR);
				graphManager.calculateCpaPoints(clickTableName, impressionTableName, serverTableName, GRANULARITY_DAY);
				graphManager.calculateCpaPoints(clickTableName, impressionTableName, serverTableName, GRANULARITY_WEEK);
			}
			case CALCULATE_BOUNCE_RATE -> {
				graphManager.calculateBRatePoints(clickTableName, serverTableName, GRANULARITY_HOUR);
				graphManager.calculateBRatePoints(clickTableName, serverTableName, GRANULARITY_DAY);
				graphManager.calculateBRatePoints(clickTableName, serverTableName, GRANULARITY_WEEK);
			}
			case CALCULATE_DISTRIBUTION -> {
				graphManager.calculateDistributionPoints(clickTableName, serverTableName);
			}
		}
		return null;
	}
}
