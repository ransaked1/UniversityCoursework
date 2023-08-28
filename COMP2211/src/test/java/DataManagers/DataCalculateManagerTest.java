package DataManagers;

import org.junit.jupiter.api.*;

import java.sql.SQLException;

import static org.junit.jupiter.api.Assertions.*;

class DataCalculateManagerTest {
	static CSVtoSQLReader reader = new CSVtoSQLReader("input_db_test");

	DataCalculateManager calcManager = new DataCalculateManager("input_db_test");

	@BeforeAll
	static void beforeAll() throws SQLException {
		reader.dropTable("test_impression_log");
		reader.dropTable("test_server_log");
		reader.dropTable("test_click_log");
		reader.writeLogToTable("input/impression_log.csv", "test_impression_log");
		reader.writeLogToTable("input/server_log.csv", "test_server_log");
		reader.writeLogToTable("input/click_log.csv", "test_click_log");
		reader.addFilterColumnsFromImpressionTable("test_click_log", "test_impression_log");
		reader.addFilterColumnsFromImpressionTable("test_server_log", "test_impression_log");
	}

	@BeforeEach
	void setUp() {
		calcManager.resetFilters();
	}

	@Test
	@DisplayName("Checking CTR calculations")
	void calculateCTRTest() {
		assertTrue(calcManager.calculateCTR("test_click_log", "test_impression_log"));
		assertEquals(4.92, calcManager.getCtr());

		calcManager.setTimeFilterStart("2015-01-13 00:00:00");
		calcManager.setTimeFilterEnd("2015-01-13 23:59:59");
		assertTrue(calcManager.calculateCTR("test_click_log", "test_impression_log"));
		assertEquals(4.86, calcManager.getCtr());

		calcManager.setTimeFilterStart("2015-01-13 16:30:00");
		calcManager.setTimeFilterEnd("2015-01-13 16:31:00");
		assertTrue(calcManager.calculateCTR("test_click_log", "test_impression_log"));
		assertEquals(4.44, calcManager.getCtr());
	}

	@Test
	@DisplayName("Checking CPM calculations")
	void calculateCPMTest() {
		assertTrue(calcManager.calculateCPM("test_click_log", "test_impression_log"));
		assertEquals(242.94, calcManager.getCpm());

		calcManager.setTimeFilterStart("2015-01-13 00:00:00");
		calcManager.setTimeFilterEnd("2015-01-13 23:59:59");
		assertTrue(calcManager.calculateCPM("test_click_log", "test_impression_log"));
		assertEquals(241.35, calcManager.getCpm());

		calcManager.setTimeFilterStart("2015-01-13 16:30:00");
		calcManager.setTimeFilterEnd("2015-01-13 16:31:00");
		assertTrue(calcManager.calculateCPM("test_click_log", "test_impression_log"));
		assertEquals(155.33, calcManager.getCpm());
	}

	@Test
	@DisplayName("Checking CPA calculations")
	void calculateCPATest() {
		assertTrue(calcManager.calculateCPA("test_click_log", "test_impression_log", "test_server_log"));
		assertEquals(58.29, calcManager.getCpa());

		calcManager.setTimeFilterStart("2015-01-13 00:00:00");
		calcManager.setTimeFilterEnd("2015-01-13 23:59:59");
		assertTrue(calcManager.calculateCPA("test_click_log", "test_impression_log", "test_server_log"));
		assertEquals(72.16, calcManager.getCpa());

	}

	@Test
	@DisplayName("Checking CPC calculations")
	void calculateCPCTest() {
		assertTrue(calcManager.calculateCPC("test_click_log"));
		assertEquals(4.91, calcManager.getCpc());

		calcManager.setTimeFilterStart("2015-01-13 00:00:00");
		calcManager.setTimeFilterEnd("2015-01-13 23:59:59");
		assertTrue(calcManager.calculateCPC("test_click_log"));
		assertEquals(4.93, calcManager.getCpc());

		calcManager.setTimeFilterStart("2015-01-13 16:30:00");
		calcManager.setTimeFilterEnd("2015-01-13 16:31:00");
		assertTrue(calcManager.calculateCPC("test_click_log"));
		assertEquals(3.47, calcManager.getCpc());
	}

	@Test
	@DisplayName("Checking Bounce Rate calculations")
	void calculateBounceRateTest() {
		assertTrue(calcManager.calculateBounceRate("test_click_log", "test_server_log"));
		assertEquals(25.78, calcManager.getBounceRate());

		calcManager.setTimeFilterStart("2015-01-13 00:00:00");
		calcManager.setTimeFilterEnd("2015-01-13 23:59:59");
		assertTrue(calcManager.calculateBounceRate("test_click_log", "test_server_log"));
		assertEquals(27.71, calcManager.getBounceRate());

		calcManager.setTimeFilterStart("2015-01-13 16:30:00");
		calcManager.setTimeFilterEnd("2015-01-13 16:31:00");
		assertTrue(calcManager.calculateBounceRate("test_click_log", "test_server_log"));
		assertEquals(0.0, calcManager.getBounceRate());
	}

	@AfterAll
	static void afterAll() throws SQLException {
		reader.dropTable("test_impression_log");
		reader.dropTable("test_click_log");
		reader.dropTable("test_server_log");
	}
}