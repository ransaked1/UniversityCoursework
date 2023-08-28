package DataManagers;

/**
 * Class manages the metrics that need mathematical calculations, it is an extension of the fetching operations
 */
public class DataCalculateManager extends DataFetchManager {
	private static Double ctr = 0.0;
	private static Double cpm = 0.0;
	private static Double cpa = 0.0;
	private static Double cpc = 0.0;
	private static Double bounceRate = 0.0;

	public DataCalculateManager(String databaseName) {
		super(databaseName);
	}

	/**
	 * Method fetches the clicks and impressions counts then calculates and rounds: CTR = clicks / impressions
	 *
	 * @param clicksTable
	 * @param impressionTable
	 * @return
	 */
	public boolean calculateCTR(String clicksTable, String impressionTable) {
		try {
			fetchClicks(clicksTable);
			fetchImpressions(impressionTable);
			ctr = this.getClicks().doubleValue() / this.getImpressions().doubleValue();
			ctr = Math.floor(ctr * 10000) / 100;
			return true;
		} catch (Exception e) {
			return false;
		}
	}

	/**
	 * Method fetches the campaign cost and impressions count then calculates and rounds: CPM = 1000 * cost / impressions
	 *
	 * @param clicksTable
	 * @param impressionTable
	 * @return
	 */
	public boolean calculateCPM(String clicksTable, String impressionTable) {
		try {
			fetchImpressions(impressionTable);
			fetchCost(clicksTable, impressionTable);
			cpm = 1000 * this.getCost() / this.getImpressions().doubleValue();
			cpm = Math.floor(cpm * 100) / 100;
			return true;
		} catch (Exception e) {
			return false;
		}
	}

	/**
	 * Method fetches the campaign cost and clicks count then calculates and rounds: CPC = clicks cost / clicks
	 *
	 * @param clicksTable
	 * @return
	 */
	public boolean calculateCPC(String clicksTable) {
		try {
			fetchClicks(clicksTable);
			fetchClicksCost(clicksTable);
			cpc = this.getClicksCost() / this.getClicks().doubleValue();
			cpc = Math.floor(cpc * 100) / 100;
			return true;
		} catch (Exception e) {
			return false;
		}
	}

	/**
	 * Method fetches the campaign cost and conversions count then calculates and rounds: CPA = cost / conversions
	 *
	 * @param clicksTable
	 * @param impressionTable
	 * @param serverTable
	 * @return
	 */
	public boolean calculateCPA(String clicksTable, String impressionTable, String serverTable) {
		try {
			fetchConversions(serverTable);
			fetchCost(clicksTable, impressionTable);
			cpa = this.getCost() / this.getConversions().doubleValue();
			cpa = Math.floor(cpa * 100) / 100;
			return true;
		} catch (Exception e) {
			return false;
		}
	}

	/**
	 * Method fetches the bounces and clicks counts then calculates and rounds: Bounce Rate = bounces / clicks
	 *
	 * @param clicksTable
	 * @param serverTable
	 * @return
	 */
	public boolean calculateBounceRate(String clicksTable, String serverTable) {
		try {
			fetchClicks(clicksTable);
			fetchBounces(serverTable);
			bounceRate = this.getBounces().doubleValue() / this.getClicks().doubleValue();
			bounceRate = Math.floor(bounceRate * 10000) / 100;
			return true;
		} catch (Exception e) {
			return false;
		}
	}

	/**
	 * Executes all the calculations available
	 *
	 * @param impressionTableName
	 * @param clickTableName
	 * @param serverTableName
	 */
	public void calculateAll(String impressionTableName, String clickTableName, String serverTableName) {
		calculateCTR(clickTableName, impressionTableName);
		calculateCPM(clickTableName, impressionTableName);
		calculateCPC(clickTableName);
		calculateCPA(clickTableName, impressionTableName, serverTableName);
		calculateBounceRate(clickTableName, serverTableName);
	}

	/**
	 * Executes all the calculations and count fetching operations
	 *
	 * @param impressionTableName
	 * @param clickTableName
	 * @param serverTableName
	 */
	public void compileAll(String impressionTableName, String clickTableName, String serverTableName) {
		fetchAll(impressionTableName, clickTableName, serverTableName);
		calculateAll(impressionTableName, clickTableName, serverTableName);
	}

	public static Double getCpc() {
		return cpc;
	}

	public static Double getCtr() {
		return ctr;
	}

	public static Double getCpm() {
		return cpm;
	}

	public static Double getCpa() {
		return cpa;
	}

	public static Double getBounceRate() {
		return bounceRate;
	}
}
