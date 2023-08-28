package DataManagers;

import javafx.util.Pair;

import java.sql.*;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Locale;

import static java.lang.Math.ceil;

public class DataGraphManager extends DataFetchManager {

	private static final int GRANULARITY_HOUR = 0;
	private static final int GRANULARITY_DAY = 1;
	private static final int GRANULARITY_WEEK = 2;

	final DateTimeFormatter formatterIn = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss", Locale.ENGLISH);
	final DateTimeFormatter formatterHourOut = DateTimeFormatter.ofPattern("MM-dd HH:00", Locale.ENGLISH);
	final DateTimeFormatter formatterDayOut = DateTimeFormatter.ofPattern("yyyy-MM-dd", Locale.ENGLISH);

	private static ArrayList<Pair<String,Integer>> impressionsPointsHour = new ArrayList<>();
	private static ArrayList<Pair<String,Integer>> impressionsPointsDay = new ArrayList<>();
	private static ArrayList<Pair<String,Integer>> impressionsPointsWeek = new ArrayList<>();

	private static ArrayList<Pair<String,Integer>> clicksPointsHour = new ArrayList<>();
	private static ArrayList<Pair<String,Integer>> clicksPointsDay = new ArrayList<>();
	private static ArrayList<Pair<String,Integer>> clicksPointsWeek = new ArrayList<>();

	private static ArrayList<Pair<String,Integer>> uniquesPointsHour = new ArrayList<>();
	private static ArrayList<Pair<String,Integer>> uniquesPointsDay = new ArrayList<>();
	private static ArrayList<Pair<String,Integer>> uniquesPointsWeek = new ArrayList<>();

	private static ArrayList<Pair<String,Integer>> bouncesPointsHour = new ArrayList<>();
	private static ArrayList<Pair<String,Integer>> bouncesPointsDay = new ArrayList<>();
	private static ArrayList<Pair<String,Integer>> bouncesPointsWeek = new ArrayList<>();

	private static ArrayList<Pair<String,Integer>> conversionsPointsHour = new ArrayList<>();
	private static ArrayList<Pair<String,Integer>> conversionsPointsDay = new ArrayList<>();
	private static ArrayList<Pair<String,Integer>> conversionsPointsWeek = new ArrayList<>();

	private static ArrayList<Pair<String,Double>> costsPointsHour = new ArrayList<>();
	private static ArrayList<Pair<String,Double>> costsPointsDay = new ArrayList<>();
	private static ArrayList<Pair<String,Double>> costsPointsWeek = new ArrayList<>();

	private static ArrayList<Pair<String,Double>> ctrPointsHour = new ArrayList<>();
	private static ArrayList<Pair<String,Double>> ctrPointsDay = new ArrayList<>();
	private static ArrayList<Pair<String,Double>> ctrPointsWeek = new ArrayList<>();

	private static ArrayList<Pair<String,Double>> cpmPointsHour = new ArrayList<>();
	private static ArrayList<Pair<String,Double>> cpmPointsDay = new ArrayList<>();
	private static ArrayList<Pair<String,Double>> cpmPointsWeek = new ArrayList<>();

	private static ArrayList<Pair<String,Double>> cpcPointsHour = new ArrayList<>();
	private static ArrayList<Pair<String,Double>> cpcPointsDay = new ArrayList<>();
	private static ArrayList<Pair<String,Double>> cpcPointsWeek = new ArrayList<>();

	private static ArrayList<Pair<String,Double>> cpaPointsHour = new ArrayList<>();
	private static ArrayList<Pair<String,Double>> cpaPointsDay = new ArrayList<>();
	private static ArrayList<Pair<String,Double>> cpaPointsWeek = new ArrayList<>();

	private static ArrayList<Pair<String,Double>> bRatePointsHour = new ArrayList<>();
	private static ArrayList<Pair<String,Double>> bRatePointsDay = new ArrayList<>();
	private static ArrayList<Pair<String,Double>> bRatePointsWeek = new ArrayList<>();

	private static ArrayList<Pair<String,Integer>> distributionPoints = new ArrayList<>();


	public DataGraphManager(String databaseName) {
		super(databaseName);
	}


	public void fetchImpressionsPoints(String impressionTableName, int granularity) throws SQLException {
		String sql = "";
		if (granularity == 0) {
			sql = "SELECT strftime('%d %H', date) AS valDay, strftime('%m-%d %H:00', date) AS date,COUNT(*) AS count FROM " + impressionTableName + " WHERE id IS NOT NULL ";

			sql = addTimeFiltersDateColumn(sql);
			sql = addFilters(sql);

			sql += " GROUP BY valDay";
		}

		if (granularity == 1 || granularity == 2) {
			sql = "SELECT strftime('%d', date) AS valDay, strftime('%Y-%m-%d', date) AS date,COUNT(*) AS count FROM " + impressionTableName + " WHERE id IS NOT NULL ";

			sql = addTimeFiltersDateColumn(sql);
			sql = addFilters(sql);

			sql += " GROUP BY valDay";
		}

		conn = DriverManager.getConnection(url);
		Statement stmt = conn.createStatement();
		ResultSet rs = stmt.executeQuery(sql);

		if (granularity == GRANULARITY_HOUR) {
			impressionsPointsHour = new ArrayList<>();
			while (rs.next()) {
				impressionsPointsHour.add(new Pair<>(rs.getString("date"), rs.getInt("count")));
			}
			addZeroValueHoursInt(impressionsPointsHour);
		}

		if (granularity == GRANULARITY_DAY) {
			impressionsPointsDay = new ArrayList<>();
			while (rs.next()) {
				impressionsPointsDay.add(new Pair<>(rs.getString("date"), rs.getInt("count")));
			}
			addZeroValueDaysInt(impressionsPointsDay);
		}

		if (granularity == GRANULARITY_WEEK) {
			impressionsPointsWeek = new ArrayList<>();
			int i = 1;
			int sum = 0;

			String lastDate = "";
			int lastSum = 0;

			while (rs.next()) {
				sum += rs.getInt("count");
				lastDate = rs.getString("date");
				lastSum = sum;

				if (i % 7 == 0) {
					//System.out.println(i + " " + sum);
					impressionsPointsWeek.add(new Pair<>(rs.getString("date"), sum));
					i = 0;
					sum = 0;
				}
				//System.out.println(i + " " + sum);
				i++;
			}
			impressionsPointsWeek.add(new Pair<>(lastDate, lastSum));
			addZeroValueWeeksInt(impressionsPointsWeek);
		}
		conn.close();
	}

	public void fetchClicksPoints(String clickTableName, int granularity) throws SQLException {
		String sql = "";
		if (granularity == 0) {
			sql = "SELECT strftime('%d %H', date) AS valDay, strftime('%m-%d %H:00', date) AS date,COUNT(*) AS count FROM " + clickTableName + " WHERE id IS NOT NULL ";

			sql = addTimeFiltersDateColumn(sql);
			sql = addFilters(sql);

			sql += " GROUP BY valDay";
		}

		if (granularity == 1 || granularity == 2) {
			sql = "SELECT strftime('%d', date) AS valDay, strftime('%Y-%m-%d', date) AS date,COUNT(*) AS count FROM " + clickTableName + " WHERE id IS NOT NULL ";

			sql = addTimeFiltersDateColumn(sql);
			sql = addFilters(sql);

			sql += " GROUP BY valDay";
		}

		conn = DriverManager.getConnection(url);
		Statement stmt = conn.createStatement();
		ResultSet rs = stmt.executeQuery(sql);

		if (granularity == GRANULARITY_HOUR) {
			clicksPointsHour = new ArrayList<>();
			while (rs.next()) {
				clicksPointsHour.add(new Pair<>(rs.getString("date"), rs.getInt("count")));
			}
			addZeroValueHoursInt(clicksPointsHour);
		}

		if (granularity == GRANULARITY_DAY) {
			clicksPointsDay = new ArrayList<>();
			while (rs.next()) {
				clicksPointsDay.add(new Pair<>(rs.getString("date"), rs.getInt("count")));
			}
			addZeroValueDaysInt(clicksPointsDay);
		}

		if (granularity == GRANULARITY_WEEK) {
			clicksPointsWeek = new ArrayList<>();
			int i = 1;
			int sum = 0;

			String lastDate = "";
			int lastSum = 0;

			while (rs.next()) {
				sum += rs.getInt("count");
				lastDate = rs.getString("date");
				lastSum = sum;

				if (i % 7 == 0) {
					//System.out.println(i + " " + sum);
					clicksPointsWeek.add(new Pair<>(rs.getString("date"), sum));
					i = 0;
					sum = 0;
				}
				//System.out.println(i + " " + sum);
				i++;
			}
			clicksPointsWeek.add(new Pair<>(lastDate, lastSum));
			addZeroValueWeeksInt(clicksPointsWeek);
		}
		//System.out.println(impressionsPointsWeek);
		conn.close();
	}

	public void fetchUniquesPoints(String clickTableName, int granularity) throws SQLException {
		String sql = "";
		if (granularity == 0) {
			sql = "SELECT strftime('%d %H', date) AS valDay, strftime('%m-%d %H:00', date) AS date,COUNT(DISTINCT id) AS count FROM " + clickTableName + " WHERE 'id:1' IS NOT NULL ";

			sql = addTimeFiltersDateColumn(sql);
			sql = addFilters(sql);

			sql += " GROUP BY valDay";
		}

		if (granularity == 1 || granularity == 2) {
			sql = "SELECT strftime('%d', date) AS valDay, strftime('%Y-%m-%d', date) AS date,COUNT(DISTINCT id) AS count FROM " + clickTableName + " WHERE 'id:1' IS NOT NULL ";

			sql = addTimeFiltersDateColumn(sql);
			sql = addFilters(sql);

			sql += " GROUP BY valDay";
		}

		conn = DriverManager.getConnection(url);
		Statement stmt = conn.createStatement();
		ResultSet rs = stmt.executeQuery(sql);

		if (granularity == GRANULARITY_HOUR) {
			uniquesPointsHour = new ArrayList<>();
			while (rs.next()) {
				uniquesPointsHour.add(new Pair<>(rs.getString("date"), rs.getInt("count")));
			}
			addZeroValueHoursInt(uniquesPointsHour);
		}

		if (granularity == GRANULARITY_DAY) {
			uniquesPointsDay = new ArrayList<>();
			while (rs.next()) {
				uniquesPointsDay.add(new Pair<>(rs.getString("date"), rs.getInt("count")));
			}
			addZeroValueDaysInt(uniquesPointsDay);
		}

		if (granularity == GRANULARITY_WEEK) {
			uniquesPointsWeek = new ArrayList<>();
			int i = 1;
			int sum = 0;

			String lastDate = "";
			int lastSum = 0;

			while (rs.next()) {
				sum += rs.getInt("count");
				lastDate = rs.getString("date");
				lastSum = sum;

				if (i % 7 == 0) {
					//System.out.println(i + " " + sum);
					uniquesPointsWeek.add(new Pair<>(rs.getString("date"), sum));
					i = 0;
					sum = 0;
				}
				//System.out.println(i + " " + sum);
				i++;
			}
			uniquesPointsWeek.add(new Pair<>(lastDate, lastSum));
			addZeroValueWeeksInt(uniquesPointsWeek);
		}
		//System.out.println(impressionsPointsWeek);
		conn.close();
	}

	public void fetchBouncesPoints(String serverTableName, int granularity) throws SQLException {
		String sql = "";
		if (bounceFilter == 0) {
			if (granularity == 0) {
				sql = "SELECT strftime('%d %H', entry_date) AS valDay, strftime('%m-%d %H:00', entry_date) AS date,COUNT(*) AS count FROM " + serverTableName + " WHERE ((julianday(exit_date) - julianday(entry_date)) * 86400.0) < 30 ";

				sql = addTimeFiltersDoubleDateColumn(sql);
				sql = addFilters(sql);

				sql += " GROUP BY valDay";
			}

			if (granularity == 1 || granularity == 2) {
				sql = "SELECT strftime('%d', entry_date) AS valDay, strftime('%Y-%m-%d', entry_date) AS date,COUNT(*) AS count FROM " + serverTableName + " WHERE ((julianday(exit_date) - julianday(entry_date)) * 86400.0) < 30 ";

				sql = addTimeFiltersDoubleDateColumn(sql);
				sql = addFilters(sql);

				sql += " GROUP BY valDay";
			}
		}
		if (bounceFilter == 1) {
			if (granularity == 0) {
				sql = "SELECT strftime('%d %H', entry_date) AS valDay, strftime('%m-%d %H:00', entry_date) AS date,COUNT(*) AS count FROM " + serverTableName + " WHERE pages_viewed = '1' ";

				sql = addTimeFiltersDoubleDateColumn(sql);
				sql = addFilters(sql);

				sql += " GROUP BY valDay";
			}

			if (granularity == 1 || granularity == 2) {
				sql = "SELECT strftime('%d', entry_date) AS valDay, strftime('%Y-%m-%d', entry_date) AS date,COUNT(*) AS count FROM " + serverTableName + " WHERE pages_viewed = '1' ";

				sql = addTimeFiltersDoubleDateColumn(sql);
				sql = addFilters(sql);

				sql += " GROUP BY valDay";
			}
		}

		conn = DriverManager.getConnection(url);
		Statement stmt = conn.createStatement();
		ResultSet rs = stmt.executeQuery(sql);

		if (granularity == GRANULARITY_HOUR) {
			bouncesPointsHour = new ArrayList<>();
			while (rs.next()) {
				bouncesPointsHour.add(new Pair<>(rs.getString("date"), rs.getInt("count")));
			}
			addZeroValueHoursInt(bouncesPointsHour);
		}

		if (granularity == GRANULARITY_DAY) {
			bouncesPointsDay = new ArrayList<>();
			while (rs.next()) {
				bouncesPointsDay.add(new Pair<>(rs.getString("date"), rs.getInt("count")));
			}
			addZeroValueDaysInt(bouncesPointsDay);
		}

		if (granularity == GRANULARITY_WEEK) {
			bouncesPointsWeek = new ArrayList<>();
			int i = 1;
			int sum = 0;

			String lastDate = "";
			int lastSum = 0;

			while (rs.next()) {
				sum += rs.getInt("count");
				lastDate = rs.getString("date");
				lastSum = sum;

				if (i % 7 == 0) {
					//System.out.println(i + " " + sum);
					bouncesPointsWeek.add(new Pair<>(rs.getString("date"), sum));
					i = 0;
					sum = 0;
				}
				//System.out.println(i + " " + sum);
				i++;
			}
			bouncesPointsWeek.add(new Pair<>(lastDate, lastSum));
			addZeroValueWeeksInt(bouncesPointsWeek);
		}
		//System.out.println(impressionsPointsWeek);
		conn.close();
	}

	public void fetchConversionsPoints(String serverTableName, int granularity) throws SQLException {
		String sql = "";
		if (granularity == 0) {
			sql = "SELECT strftime('%d %H', entry_date) AS valDay, strftime('%m-%d %H:00', entry_date) AS date,COUNT(*) AS count FROM " + serverTableName + " WHERE conversion = 'Yes'";

			sql = addTimeFiltersDoubleDateColumn(sql);
			sql = addFilters(sql);

			sql += " GROUP BY valDay";
		}

		if (granularity == 1 || granularity == 2) {
			sql = "SELECT strftime('%d', entry_date) AS valDay, strftime('%Y-%m-%d', entry_date) AS date,COUNT(*) AS count FROM " + serverTableName + " WHERE conversion = 'Yes'";

			sql = addTimeFiltersDoubleDateColumn(sql);
			sql = addFilters(sql);

			sql += " GROUP BY valDay";
		}

		conn = DriverManager.getConnection(url);
		Statement stmt = conn.createStatement();
		ResultSet rs = stmt.executeQuery(sql);

		if (granularity == GRANULARITY_HOUR) {
			conversionsPointsHour = new ArrayList<>();
			while (rs.next()) {
				conversionsPointsHour.add(new Pair<>(rs.getString("date"), rs.getInt("count")));
			}
			addZeroValueHoursInt(conversionsPointsHour);
		}

		if (granularity == GRANULARITY_DAY) {
			conversionsPointsDay = new ArrayList<>();
			while (rs.next()) {
				conversionsPointsDay.add(new Pair<>(rs.getString("date"), rs.getInt("count")));
			}
			addZeroValueDaysInt(conversionsPointsDay);
		}

		if (granularity == GRANULARITY_WEEK) {
			conversionsPointsWeek = new ArrayList<>();
			int i = 1;
			int sum = 0;

			String lastDate = "";
			int lastSum = 0;

			while (rs.next()) {
				sum += rs.getInt("count");
				lastDate = rs.getString("date");
				lastSum = sum;

				if (i % 7 == 0) {
					conversionsPointsWeek.add(new Pair<>(rs.getString("date"), sum));
					i = 0;
					sum = 0;
				}
				i++;
			}
			conversionsPointsWeek.add(new Pair<>(lastDate, lastSum));
			addZeroValueWeeksInt(conversionsPointsWeek);
		}
		conn.close();
	}

	public void fetchCostsPoints(String clickTableName, String impressionsTableName, int granularity) throws SQLException {
		String sql = "";
		if (granularity == 0) {
			sql = "SELECT valDayAndHour1 AS valDayAndHour, date, (c_cost + i_cost) AS cost FROM "
			+ "((SELECT strftime('%d,%H', date) AS valDayAndHour1, strftime('%m-%d %H:00', date) AS date, SUM(click_cost) AS c_cost FROM " + clickTableName + " WHERE click_cost IS NOT NULL";

			sql = addTimeFiltersDateColumn(sql);
			sql = addFilters(sql);
			sql += " GROUP BY valDayAndHour1)";

			sql += " JOIN ((SELECT strftime('%d,%H', date) AS valDayAndHour2, SUM(impression_cost) AS i_cost FROM " + impressionsTableName + " WHERE impression_cost IS NOT NULL";

			sql = addTimeFiltersDateColumn(sql);
			sql = addFilters(sql);
			sql += " GROUP BY valDayAndHour2)) ON valDayAndHour2 = valDayAndHour1)";
		}

		if (granularity == 1 || granularity == 2) {
			sql = "SELECT valDayAndHour1 AS valDayAndHour, date,(c_cost + i_cost) AS cost FROM "
					+ "((SELECT strftime('%d', date) AS valDayAndHour1, strftime('%Y-%m-%d', date) AS date, SUM(click_cost) AS c_cost FROM " + clickTableName + " WHERE click_cost IS NOT NULL";

			sql = addTimeFiltersDateColumn(sql);
			sql = addFilters(sql);
			sql += " GROUP BY valDayAndHour1)";

			sql += " JOIN ((SELECT strftime('%d', date) AS valDayAndHour2, SUM(impression_cost) AS i_cost FROM " + impressionsTableName + " WHERE impression_cost IS NOT NULL";

			sql = addTimeFiltersDateColumn(sql);
			sql = addFilters(sql);
			sql += " GROUP BY valDayAndHour2)) ON valDayAndHour2 = valDayAndHour1)";
		}

		conn = DriverManager.getConnection(url);
		Statement stmt = conn.createStatement();
		ResultSet rs = stmt.executeQuery(sql);

		if (granularity == GRANULARITY_HOUR) {
			costsPointsHour = new ArrayList<>();
			while (rs.next()) {
				costsPointsHour.add(new Pair<>(rs.getString("date"), Math.floor(rs.getDouble("cost") * 100) / 100 ));
			}
			addZeroValueHoursDouble(costsPointsHour);
		}

		if (granularity == GRANULARITY_DAY) {
			costsPointsDay = new ArrayList<>();
			while (rs.next()) {
				costsPointsDay.add(new Pair<>(rs.getString("date"), Math.floor(rs.getDouble("cost") * 100) / 100 ));
			}
			addZeroValueDaysDouble(costsPointsDay);
		}

		if (granularity == GRANULARITY_WEEK) {
			costsPointsWeek = new ArrayList<>();
			int i = 1;
			double sum = 0;

			String lastDate = "";
			double lastSum = 0;

			while (rs.next()) {
				sum += rs.getDouble("cost");
				sum = Math.floor(sum * 100) / 100;
				lastDate = rs.getString("date");
				lastSum = sum;

				if (i % 7 == 0) {
					costsPointsWeek.add(new Pair<>(rs.getString("date"), sum));
					i = 0;
					sum = 0;
				}
				i++;
			}
			costsPointsWeek.add(new Pair<>(lastDate, lastSum));
			addZeroValueWeeksDouble(costsPointsWeek);
		}
		//System.out.println(impressionsPointsWeek);
		conn.close();
	}

	public void calculateCtrPoints(String clickTable, String impressionsTable, int granularity) throws SQLException {
		//fetchClicksPoints(clickTable, granularity);
		//fetchImpressionsPoints(impressionsTable, granularity);
		if (granularity == 0) {
			ctrPointsHour = new ArrayList<>();
			double ctr;
			for (int i = 0; i < clicksPointsHour.size(); i++) {
				if (impressionsPointsHour.get(i).getValue() != 0)
					ctr = clicksPointsHour.get(i).getValue().doubleValue() / impressionsPointsHour.get(i).getValue();
				else
					ctr = 0;
				ctr = Math.floor(ctr * 10000) / 100;
				ctrPointsHour.add(new Pair<>(clicksPointsHour.get(i).getKey(), ctr));
			}
		}
		if (granularity == 1) {
			ctrPointsDay = new ArrayList<>();
			double ctr;
			for (int i = 0; i < clicksPointsDay.size(); i++) {
				if (impressionsPointsDay.get(i).getValue() != 0)
					ctr = clicksPointsDay.get(i).getValue().doubleValue() / impressionsPointsDay.get(i).getValue();
				else
					ctr = 0;
				ctr = Math.floor(ctr * 10000) / 100;
				ctrPointsDay.add(new Pair<>(clicksPointsDay.get(i).getKey(), ctr));
			}
		}
		if (granularity == 2) {
			ctrPointsWeek = new ArrayList<>();
			double ctr;
			for (int i = 0; i < clicksPointsWeek.size(); i++) {
				if (impressionsPointsWeek.get(i).getValue() != 0)
					ctr = clicksPointsWeek.get(i).getValue().doubleValue() / impressionsPointsWeek.get(i).getValue();
				else
					ctr = 0;
				ctr = Math.floor(ctr * 10000) / 100;
				ctrPointsWeek.add(new Pair<>(clicksPointsWeek.get(i).getKey(), ctr));
			}
		}
	}

	public void calculateCpmPoints(String clickTable, String impressionsTable, int granularity) throws SQLException {
		//fetchImpressionsPoints(impressionsTable, granularity);
		//fetchCostsPoints(clickTable, impressionsTable, granularity);
		if (granularity == 0) {
			cpmPointsHour = new ArrayList<>();
			double cpm;
			for (int i = 0; i < costsPointsHour.size(); i++) {
				if (impressionsPointsHour.get(i).getValue() != 0)
					cpm = 1000 * costsPointsHour.get(i).getValue() / impressionsPointsHour.get(i).getValue();
				else
					cpm = 0;
				cpm = Math.floor(cpm * 100) / 100;
				cpmPointsHour.add(new Pair<>(costsPointsHour.get(i).getKey(), cpm));
			}
		}
		if (granularity == 1) {
			cpmPointsDay = new ArrayList<>();
			double cpm;
			for (int i = 0; i < costsPointsDay.size(); i++) {
				if (impressionsPointsDay.get(i).getValue() != 0)
					cpm = 1000 * costsPointsDay.get(i).getValue() / impressionsPointsDay.get(i).getValue();
				else
					cpm = 0;
				cpm = Math.floor(cpm * 100) / 100;
				cpmPointsDay.add(new Pair<>(costsPointsDay.get(i).getKey(), cpm));
			}
		}
		if (granularity == 2) {
			cpmPointsWeek = new ArrayList<>();
			double cpm;
			for (int i = 0; i < costsPointsWeek.size(); i++) {
				if (impressionsPointsWeek.get(i).getValue() != 0)
					cpm = 1000 * costsPointsWeek.get(i).getValue() / impressionsPointsWeek.get(i).getValue();
				else
					cpm = 0;
				cpm = Math.floor(cpm * 100) / 100;
				cpmPointsWeek.add(new Pair<>(costsPointsWeek.get(i).getKey(), cpm));
			}
		}
	}

	public void calculateCpcPoints(String clickTable, String impressionsTable, int granularity) throws SQLException {
		//fetchClicksPoints(clickTable, granularity);
		//fetchCostsPoints(clickTable, impressionsTable, granularity);
		if (granularity == 0) {
			cpcPointsHour = new ArrayList<>();
			double cpc;
			for (int i = 0; i < costsPointsHour.size(); i++) {
				if (clicksPointsHour.get(i).getValue() != 0)
					cpc = costsPointsHour.get(i).getValue() / clicksPointsHour.get(i).getValue();
				else
					cpc = 0;
				cpc = Math.floor(cpc * 100) / 100;
				cpcPointsHour.add(new Pair<>(costsPointsHour.get(i).getKey(), cpc));
			}
		}
		if (granularity == 1) {
			cpcPointsDay = new ArrayList<>();
			double cpc;
			for (int i = 0; i < costsPointsDay.size(); i++) {
				if (clicksPointsDay.get(i).getValue() != 0)
					cpc = costsPointsDay.get(i).getValue() / clicksPointsDay.get(i).getValue();
				else
					cpc = 0;
				cpc = Math.floor(cpc * 100) / 100;
				cpcPointsDay.add(new Pair<>(costsPointsDay.get(i).getKey(), cpc));
			}
		}
		if (granularity == 2) {
			cpcPointsWeek = new ArrayList<>();
			double cpc;
			for (int i = 0; i < costsPointsWeek.size(); i++) {
				if (clicksPointsWeek.get(i).getValue() != 0)
					cpc = costsPointsWeek.get(i).getValue() / clicksPointsWeek.get(i).getValue();
				else
					cpc = 0;
				cpc = Math.floor(cpc * 100) / 100;
				cpcPointsWeek.add(new Pair<>(costsPointsWeek.get(i).getKey(), cpc));
			}
		}
	}

	public void calculateCpaPoints(String clickTable, String impressionsTable, String serverTable, int granularity) throws SQLException {
		//fetchConversionsPoints(serverTable, granularity);
		//fetchCostsPoints(clickTable, impressionsTable, granularity);
		if (granularity == 0) {
			cpaPointsHour = new ArrayList<>();
			double cpa;
			for (int i = 0; i < costsPointsHour.size(); i++) {
				if (conversionsPointsHour.get(i).getValue() != 0)
					cpa = costsPointsHour.get(i).getValue() / conversionsPointsHour.get(i).getValue();
				else
					cpa = 0;
				cpa = Math.floor(cpa * 100) / 100;
				cpaPointsHour.add(new Pair<>(costsPointsHour.get(i).getKey(), cpa));
			}
		}
		if (granularity == 1) {
			cpaPointsDay = new ArrayList<>();
			double cpa;
			for (int i = 0; i < costsPointsDay.size(); i++) {
				if (conversionsPointsDay.get(i).getValue() != 0) {
					cpa = costsPointsDay.get(i).getValue() / conversionsPointsDay.get(i).getValue();
				} else {
					cpa = 0;
				}
				cpa = Math.floor(cpa * 100) / 100;
				cpaPointsDay.add(new Pair<>(costsPointsDay.get(i).getKey(), cpa));
			}
		}
		if (granularity == 2) {
			cpaPointsWeek = new ArrayList<>();
			double cpa;
			for (int i = 0; i < costsPointsWeek.size(); i++) {
				if (conversionsPointsWeek.get(i).getValue() != 0) {
					cpa = costsPointsWeek.get(i).getValue() / conversionsPointsWeek.get(i).getValue().doubleValue();
				} else {
					cpa = 0;
				}
				cpa = Math.floor(cpa * 100) / 100;
				cpaPointsWeek.add(new Pair<>(costsPointsWeek.get(i).getKey(), cpa));
			}
		}
	}

	public void calculateBRatePoints(String clickTable, String serverTable, int granularity) throws SQLException {
		//fetchClicksPoints(clickTable, granularity);
		//fetchBouncesPoints(serverTable, granularity);
		if (granularity == 0) {
			bRatePointsHour = new ArrayList<>();
			double bounceRate;
			for (int i = 0; i < clicksPointsHour.size(); i++) {
				if (clicksPointsHour.get(i).getValue() != 0)
					bounceRate = bouncesPointsHour.get(i).getValue().doubleValue() / clicksPointsHour.get(i).getValue();
				else
					bounceRate = 0;
				bounceRate = Math.floor(bounceRate * 10000) / 100;
				bRatePointsHour.add(new Pair<>(clicksPointsHour.get(i).getKey(), bounceRate));
			}
		}
		if (granularity == 1) {
			bRatePointsDay = new ArrayList<>();
			double bounceRate;
			for (int i = 0; i < clicksPointsDay.size(); i++) {
				if (clicksPointsDay.get(i).getValue() != 0)
					bounceRate = bouncesPointsDay.get(i).getValue().doubleValue() / clicksPointsDay.get(i).getValue();
				else
					bounceRate = 0;
				bounceRate = Math.floor(bounceRate * 10000) / 100;
				bRatePointsDay.add(new Pair<>(clicksPointsDay.get(i).getKey(), bounceRate));
			}
		}
		if (granularity == 2) {
			bRatePointsWeek = new ArrayList<>();
			double bounceRate;
			for (int i = 0; i < clicksPointsWeek.size(); i++) {
				if (clicksPointsWeek.get(i).getValue() != 0)
					bounceRate = bouncesPointsWeek.get(i).getValue().doubleValue() / clicksPointsWeek.get(i).getValue();
				else
					bounceRate = 0;
				bounceRate = Math.floor(bounceRate * 10000) / 100;
				bRatePointsWeek.add(new Pair<>(clicksPointsWeek.get(i).getKey(), bounceRate));
			}
		}
	}

	public void calculateDistributionPoints(String clickTableName, String impressionsTableName) throws SQLException{
		distributionPoints = new ArrayList<>();
		String sql = "SELECT FLOOR(MIN(CAST(click_cost as double))) AS minCost, \n"
				+ "       CEIL(MAX(CAST(click_cost as double))) AS maxCost"
				+ " FROM " + clickTableName + " WHERE click_cost IS NOT NULL ";

		sql = addTimeFiltersDateColumn(sql);
		sql = addFilters(sql);

		conn = DriverManager.getConnection(url);
		Statement stmt = conn.createStatement();
		ResultSet rs = stmt.executeQuery(sql);

		int min = rs.getInt("minCost");
		int max = rs.getInt("maxCost");
		int size = (int) ceil((max-min) / 10.0);

		String sqlClickCost = "(SELECT click_cost, CASE\n"
				+ "    WHEN CAST(click_cost as double) <= " + size * 1 + " THEN " + (min + 0) + "\n"
				+ "    WHEN CAST(click_cost as double) <= " + size * 2 + " THEN " + (min + size) + "\n"
				+ "    WHEN CAST(click_cost as double) <= " + size * 3 + " THEN " + (min + size * 2) + "\n"
				+ "    WHEN CAST(click_cost as double) <= " + size * 4 + " THEN " + (min + size * 3) + "\n"
				+ "    WHEN CAST(click_cost as double) <= " + size * 5 + " THEN " + (min + size * 4) + "\n"
				+ "    WHEN CAST(click_cost as double) <= " + size * 6 + " THEN " + (min + size * 5) + "\n"
				+ "    WHEN CAST(click_cost as double) <= " + size * 7 + " THEN " + (min + size * 6) + "\n"
				+ "    WHEN CAST(click_cost as double) <= " + size * 8 + " THEN " + (min + size * 7) + "\n"
				+ "    WHEN CAST(click_cost as double) <= " + size * 9 + " THEN " + (min + size * 8) + "\n"
				+ "    WHEN CAST(click_cost as double) <= " + size * 10 + " THEN " + (min + size * 9) + "\n"
				+ "    ELSE 0\n"
				+ "END cost_range\n"
				+ "FROM " + clickTableName + " WHERE click_cost IS NOT NULL ";

		sqlClickCost = addTimeFiltersDateColumn(sqlClickCost);
		sqlClickCost = addFilters(sqlClickCost);
		sqlClickCost += ")\n";

		sql = "SELECT cost_range, COUNT(*) AS clicks FROM\n"
				+ sqlClickCost + " GROUP BY cost_range";

		rs = stmt.executeQuery(sql);

		int i = 0;
		Pair pair;
		rs.next();

		while (i < 10) {
			try {
				pair = new Pair<>(rs.getString("cost_range"), rs.getInt("clicks"));
			} catch (Exception e) {
				break;
			}
			String keyValueCheck = (min + size * i) + "";

			if (keyValueCheck.equals(pair.getKey())) {
				distributionPoints.add(pair);
				rs.next();
			}
			else {
				distributionPoints.add(new Pair<>(keyValueCheck, 0));
			}
			i++;
		}
	}

	public ArrayList<Pair<String, Integer>> getImpressionsPoints(int granularity) {
		if (granularity == GRANULARITY_HOUR)
			return impressionsPointsHour;
		if (granularity == GRANULARITY_DAY)
			return impressionsPointsDay;
		return impressionsPointsWeek;
	}

	public ArrayList<Pair<String, Integer>> getClicksPoints(int granularity) {
		if (granularity == GRANULARITY_HOUR)
			return clicksPointsHour;
		if (granularity == GRANULARITY_DAY)
			return clicksPointsDay;
		return clicksPointsWeek;
	}

	public ArrayList<Pair<String, Integer>> getUniquesPoints(int granularity) {
		if (granularity == GRANULARITY_HOUR)
			return uniquesPointsHour;
		if (granularity == GRANULARITY_DAY)
			return uniquesPointsDay;
		return uniquesPointsWeek;
	}

	public ArrayList<Pair<String, Integer>> getBouncesPoints(int granularity) {
		if (granularity == GRANULARITY_HOUR)
			return bouncesPointsHour;
		if (granularity == GRANULARITY_DAY)
			return bouncesPointsDay;
		return bouncesPointsWeek;
	}

	public ArrayList<Pair<String, Integer>> getConversionsPoints(int granularity) {
		if (granularity == GRANULARITY_HOUR)
			return conversionsPointsHour;
		if (granularity == GRANULARITY_DAY)
			return conversionsPointsDay;
		return conversionsPointsWeek;
	}

	public ArrayList<Pair<String, Double>> getCostsPoints(int granularity) {
		if (granularity == GRANULARITY_HOUR)
			return costsPointsHour;
		if (granularity == GRANULARITY_DAY)
			return costsPointsDay;
		return costsPointsWeek;
	}

	public ArrayList<Pair<String, Double>> getCtrPoints(int granularity) {
		if (granularity == GRANULARITY_HOUR)
			return ctrPointsHour;
		if (granularity == GRANULARITY_DAY)
			return ctrPointsDay;
		return ctrPointsWeek;
	}

	public ArrayList<Pair<String, Double>> getCpmPoints(int granularity) {
		if (granularity == GRANULARITY_HOUR)
			return cpmPointsHour;
		if (granularity == GRANULARITY_DAY)
			return cpmPointsDay;
		return cpmPointsWeek;
	}

	public ArrayList<Pair<String, Double>> getCpcPoints(int granularity) {
		if (granularity == GRANULARITY_HOUR)
			return cpcPointsHour;
		if (granularity == GRANULARITY_DAY)
			return cpcPointsDay;
		return cpcPointsWeek;
	}

	public ArrayList<Pair<String, Double>> getCpaPoints(int granularity) {
		if (granularity == GRANULARITY_HOUR)
			return cpaPointsHour;
		if (granularity == GRANULARITY_DAY)
			return cpaPointsDay;
		return cpaPointsWeek;
	}

	public ArrayList<Pair<String, Double>> getBRatePoints(int granularity) {
		if (granularity == GRANULARITY_HOUR)
			return bRatePointsHour;
		if (granularity == GRANULARITY_DAY)
			return bRatePointsDay;
		return bRatePointsWeek;
	}

	public static ArrayList<Pair<String, Integer>> getDistributionPoints() {
		return distributionPoints;
	}

	private void addZeroValueHoursInt(ArrayList<Pair<String,Integer>> list) {
		LocalDateTime start = LocalDateTime.parse("2015-" + list.get(0).getKey() + ":00", formatterIn);
		LocalDateTime end = LocalDateTime.parse("2015-" + list.get(list.size() - 1).getKey() + ":00", formatterIn);

		int timeIndex = 0;
		for (LocalDateTime date = start; date.isBefore(end.plusHours(1)); date = date.plusHours(1)) {
			//System.out.println(list);
			//System.out.println(list.get(timeIndex).getKey() + " " + formatterHourOut.format((date)));
			if (!list.get(timeIndex).getKey().equals(formatterHourOut.format(date))) {
				while (!list.get(timeIndex).getKey().equals(formatterHourOut.format(date)) && date.isBefore(end.plusHours(1))) {
					//System.out.println(list.get(timeIndex).getKey() + " " + formatterHourOut.format((date)));
					list.add(timeIndex, (new Pair<>(formatterHourOut.format(date), 0)));
					timeIndex ++;
					date = date.plusHours(1);
				}
				date = date.minusHours(1);
			} else {
				timeIndex++;
			}
		}
		//System.out.println(list);
	}

	private void addZeroValueHoursDouble(ArrayList<Pair<String,Double>> list) {
		LocalDateTime start = LocalDateTime.parse("2015-" + list.get(0).getKey() + ":00", formatterIn);
		LocalDateTime end = LocalDateTime.parse("2015-" + list.get(list.size() - 1).getKey() + ":00", formatterIn);

		int timeIndex = 0;
		for (LocalDateTime date = start; date.isBefore(end.plusHours(1)); date = date.plusHours(1)) {
			if (!list.get(timeIndex).getKey().equals(formatterHourOut.format(date))) {
				while (!list.get(timeIndex).getKey().equals(formatterHourOut.format(date)) && date.isBefore(end.plusHours(1))) {
					list.add(timeIndex, (new Pair<>(formatterHourOut.format(date), 0.0)));
					timeIndex ++;
					date = date.plusHours(1);
				}
				date = date.minusHours(1);
			} else {
				timeIndex++;
			}
		}
	}

	private void addZeroValueDaysInt(ArrayList<Pair<String,Integer>> list) {
		LocalDate start = LocalDate.parse(list.get(0).getKey());
		LocalDate end = LocalDate.parse(list.get(list.size() - 1).getKey());
		//System.out.println(start);

		int timeIndex = 0;
		for (LocalDate date = start; date.isBefore(end.plusDays(1)); date = date.plusDays(1)) {
			if (!list.get(timeIndex).getKey().equals(formatterDayOut.format(date))) {
				while (!list.get(timeIndex).getKey().equals(formatterDayOut.format(date)) && date.isBefore(end.plusDays(1))) {
					list.add(timeIndex, (new Pair<>(formatterDayOut.format(date), 0)));
					timeIndex ++;
					date = date.plusDays(1);
				}
				date = date.minusDays(1);
			} else {
				timeIndex++;
			}
		}
	}

	private void addZeroValueDaysDouble(ArrayList<Pair<String,Double>> list) {
		LocalDate start = LocalDate.parse(list.get(0).getKey());
		LocalDate end = LocalDate.parse(list.get(list.size() - 1).getKey());

		int timeIndex = 0;
		for (LocalDate date = start; date.isBefore(end.plusDays(1)); date = date.plusDays(1)) {
			if (!list.get(timeIndex).getKey().equals(formatterDayOut.format(date))) {
				while (!list.get(timeIndex).getKey().equals(formatterDayOut.format(date)) && date.isBefore(end.plusDays(1))) {
					list.add(timeIndex, (new Pair<>(formatterDayOut.format(date), 0.0)));
					timeIndex ++;
					date = date.plusDays(1);
				}
				date = date.minusDays(1);
			} else {
				timeIndex++;
			}
		}
	}

	private void addZeroValueWeeksInt(ArrayList<Pair<String,Integer>> list) {
		LocalDate start = LocalDate.parse(list.get(0).getKey());
		LocalDate end = LocalDate.parse(list.get(list.size() - 1).getKey());

		int timeIndex = 0;
		for (LocalDate date = start; date.isBefore(end.plusDays(7)); date = date.plusDays(7)) {
			if (!list.get(timeIndex).getKey().equals(formatterDayOut.format(date))) {
				while (!list.get(timeIndex).getKey().equals(formatterDayOut.format(date)) && date.isBefore(end.plusDays(7))) {
					list.add(timeIndex, (new Pair<>(formatterDayOut.format(date), 0)));
					timeIndex ++;
					date = date.plusDays(7);
				}
				date = date.minusDays(7);
			} else {
				timeIndex++;
			}
		}
	}

	private void addZeroValueWeeksDouble(ArrayList<Pair<String,Double>> list) {
		LocalDate start = LocalDate.parse(list.get(0).getKey());
		LocalDate end = LocalDate.parse(list.get(list.size() - 1).getKey());

		int timeIndex = 0;
		for (LocalDate date = start; date.isBefore(end.plusDays(7)); date = date.plusDays(7)) {
			if (!list.get(timeIndex).getKey().equals(formatterDayOut.format(date))) {
				while (!list.get(timeIndex).getKey().equals(formatterDayOut.format(date)) && date.isBefore(end.plusDays(7))) {
					list.add(timeIndex, (new Pair<>(formatterDayOut.format(date), 0.0)));
					timeIndex ++;
					date = date.plusDays(7);
				}
				date = date.minusDays(7);
			} else {
				timeIndex++;
			}
		}
	}
}
