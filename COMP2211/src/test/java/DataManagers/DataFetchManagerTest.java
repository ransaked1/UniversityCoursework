package DataManagers;

import org.junit.jupiter.api.*;

import java.sql.SQLException;

import static org.junit.jupiter.api.Assertions.*;

class DataFetchManagerTest {
	static CSVtoSQLReader reader = new CSVtoSQLReader("input_db_test");
	DataFetchManager dataManager = new DataFetchManager("input_db_test");

	private static final int GENDER_FEMALE = 1;
	private static final int GENDER_MALE = 2;

	private static final int AGE_UNDER_25 = 0;
	private static final int AGE_25_TO_34 = 1;
	private static final int AGE_35_TO_44 = 2;
	private static final int AGE_45_TO_54 = 3;
	private static final int AGE_ABOVE_54 = 4;

	private static final int INCOME_LOW = 0;
	private static final int INCOME_MEDIUM = 1;
	private static final int INCOME_HIGH = 2;

	private static final int CONTEXT_SM = 0;
	private static final int CONTEXT_SHOPPING = 1;
	private static final int CONTEXT_BLOG = 2;
	private static final int CONTEXT_NEWS = 3;

	private static final int BOUNCE_TIME = 0;
	private static final int BOUNCE_SINGLE = 1;

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
		dataManager.resetFilters();
	}

	@Nested
	@DisplayName("Testing impressions")
	class Impressions {

		@Test
		@DisplayName("Checking correct results for impressions")
		void fetchImpressionsTest() {
			//dataManager.setGenderFilter(0);
			//dataManager.resetFilters();
			assertTrue(dataManager.fetchImpressions("test_impression_log"));
			assertEquals(486104, dataManager.getImpressions());

			dataManager.setTimeFilterStart("2015-01-13 00:00:00");
			dataManager.setTimeFilterEnd("2015-01-13 23:59:59");
			assertTrue(dataManager.fetchImpressions("test_impression_log"));
			assertEquals(42159, dataManager.getImpressions());

			dataManager.setTimeFilterStart("2015-01-13 16:30:00");
			dataManager.setTimeFilterEnd("2015-01-13 16:31:00");
			assertTrue(dataManager.fetchImpressions("test_impression_log"));
			assertEquals(45, dataManager.getImpressions());
		}

		@Test
		@DisplayName("Checking correct results for impressions with gender filter")
		void fetchImpressionsGenderTest() {
			dataManager.setGenderFilter(GENDER_FEMALE);
			assertTrue(dataManager.fetchImpressions("test_impression_log"));
			assertEquals(324635, dataManager.getImpressions());

			dataManager.setGenderFilter(GENDER_MALE);
			assertTrue(dataManager.fetchImpressions("test_impression_log"));
			assertEquals(161469, dataManager.getImpressions());
		}

		@Test
		@DisplayName("Checking correct results for impressions with age filter")
		void fetchImpressionsAgeTest() {
			dataManager.setAgeFilter(AGE_UNDER_25, false);
			assertTrue(dataManager.fetchImpressions("test_impression_log"));
			assertEquals(389054, dataManager.getImpressions());

			dataManager.setAgeFilter(AGE_25_TO_34, false);
			assertTrue(dataManager.fetchImpressions("test_impression_log"));
			assertEquals(267070, dataManager.getImpressions());

			dataManager.setAgeFilter(AGE_35_TO_44, false);
			assertTrue(dataManager.fetchImpressions("test_impression_log"));
			assertEquals(145296, dataManager.getImpressions());

			dataManager.setAgeFilter(AGE_45_TO_54, false);
			assertTrue(dataManager.fetchImpressions("test_impression_log"));
			assertEquals(60972, dataManager.getImpressions());

			dataManager.setAgeFilter(AGE_ABOVE_54, false);
			assertTrue(dataManager.fetchImpressions("test_impression_log"));
			assertEquals(0, dataManager.getImpressions());

			dataManager.setAgeFilter(AGE_UNDER_25, true);
			assertTrue(dataManager.fetchImpressions("test_impression_log"));
			assertEquals(97050, dataManager.getImpressions());

			dataManager.setAgeFilter(AGE_35_TO_44, true);
			assertTrue(dataManager.fetchImpressions("test_impression_log"));
			assertEquals(218824, dataManager.getImpressions());
		}

		@Test
		@DisplayName("Checking correct results for impressions with income filter")
		void fetchImpressionsIncomeTest() {
			dataManager.setIncomeFilter(INCOME_LOW, false);
			assertTrue(dataManager.fetchImpressions("test_impression_log"));
			assertEquals(340156, dataManager.getImpressions());

			dataManager.setIncomeFilter(INCOME_MEDIUM, false);
			assertTrue(dataManager.fetchImpressions("test_impression_log"));
			assertEquals(97106, dataManager.getImpressions());

			dataManager.setIncomeFilter(INCOME_HIGH, false);
			assertTrue(dataManager.fetchImpressions("test_impression_log"));
			assertEquals(0, dataManager.getImpressions());

			dataManager.setIncomeFilter(INCOME_LOW, true);
			assertTrue(dataManager.fetchImpressions("test_impression_log"));
			assertEquals(145948, dataManager.getImpressions());

			dataManager.setIncomeFilter(INCOME_MEDIUM, true);
			assertTrue(dataManager.fetchImpressions("test_impression_log"));
			assertEquals(388998, dataManager.getImpressions());

			dataManager.setIncomeFilter(INCOME_HIGH, true);
			assertTrue(dataManager.fetchImpressions("test_impression_log"));
			assertEquals(486104, dataManager.getImpressions());
		}

		@Test
		@DisplayName("Checking correct results for impressions with context filter")
		void fetchImpressionsContextTest() {
			dataManager.setContextFilter(CONTEXT_BLOG, false);
			assertTrue(dataManager.fetchImpressions("test_impression_log"));
			assertEquals(416521, dataManager.getImpressions());

			dataManager.setContextFilter(CONTEXT_NEWS, false);
			assertTrue(dataManager.fetchImpressions("test_impression_log"));
			assertEquals(277351, dataManager.getImpressions());

			dataManager.setContextFilter(CONTEXT_SHOPPING, false);
			assertTrue(dataManager.fetchImpressions("test_impression_log"));
			assertEquals(138095, dataManager.getImpressions());

			dataManager.setContextFilter(CONTEXT_SM, false);
			assertTrue(dataManager.fetchImpressions("test_impression_log"));
			assertEquals(0, dataManager.getImpressions());

			dataManager.setContextFilter(CONTEXT_NEWS, true);
			assertTrue(dataManager.fetchImpressions("test_impression_log"));
			assertEquals(139170, dataManager.getImpressions());

			dataManager.setContextFilter(CONTEXT_BLOG, true);
			assertTrue(dataManager.fetchImpressions("test_impression_log"));
			assertEquals(208753, dataManager.getImpressions());
		}

		@Test
		@DisplayName("Checking correct results for impressions with mixed filters")
		void fetchImpressionsMixedTest() {
			dataManager.setGenderFilter(GENDER_FEMALE);
			dataManager.setAgeFilter(AGE_35_TO_44,false);
			dataManager.setIncomeFilter(INCOME_LOW,false);
			dataManager.setContextFilter(CONTEXT_BLOG, false);
			assertTrue(dataManager.fetchImpressions("test_impression_log"));
			assertEquals(145469, dataManager.getImpressions());

			dataManager.setGenderFilter(GENDER_MALE);
			dataManager.setAgeFilter(AGE_ABOVE_54,false);
			dataManager.setIncomeFilter(INCOME_MEDIUM,false);
			dataManager.setContextFilter(CONTEXT_SHOPPING, false);
			assertTrue(dataManager.fetchImpressions("test_impression_log"));
			assertEquals(11581, dataManager.getImpressions());

			dataManager.setIncomeFilter(INCOME_HIGH,false);
			assertTrue(dataManager.fetchImpressions("test_impression_log"));
			assertEquals(0, dataManager.getImpressions());
		}
	}

	@Nested
	@DisplayName("Testing clicks")
	class Clicks {
		@Test
		@DisplayName("Checking correct results for clicks")
		void fetchClicksTest() {
			assertTrue(dataManager.fetchClicks("test_click_log"));
			assertEquals(23923, dataManager.getClicks());

			dataManager.setTimeFilterStart("2015-01-13 00:00:00");
			dataManager.setTimeFilterEnd("2015-01-13 23:59:59");
			assertTrue(dataManager.fetchClicks("test_click_log"));
			assertEquals(2053, dataManager.getClicks());

			dataManager.setTimeFilterStart("2015-01-13 16:30:00");
			dataManager.setTimeFilterEnd("2015-01-13 16:31:00");
			assertTrue(dataManager.fetchClicks("test_click_log"));
			assertEquals(2, dataManager.getClicks());
		}

		@Test
		@DisplayName("Checking correct results for clicks with gender filter")
		void fetchClicksGenderTest() {
			dataManager.setGenderFilter(GENDER_FEMALE);
			assertTrue(dataManager.fetchClicks("test_click_log"));
			assertEquals(15935, dataManager.getClicks());

			dataManager.setGenderFilter(GENDER_MALE);
			assertTrue(dataManager.fetchClicks("test_click_log"));
			assertEquals(7988, dataManager.getClicks());
		}

		@Test
		@DisplayName("Checking correct results for clicks with age filter")
		void fetchClicksAgeTest() {
			dataManager.setAgeFilter(AGE_UNDER_25,false);
			assertTrue(dataManager.fetchClicks("test_click_log"));
			assertEquals(20938, dataManager.getClicks());

			dataManager.setAgeFilter(AGE_25_TO_34,false);
			assertTrue(dataManager.fetchClicks("test_click_log"));
			assertEquals(13907, dataManager.getClicks());

			dataManager.setAgeFilter(AGE_35_TO_44,false);
			assertTrue(dataManager.fetchClicks("test_click_log"));
			assertEquals(6799, dataManager.getClicks());

			dataManager.setAgeFilter(AGE_45_TO_54,false);
			assertTrue(dataManager.fetchClicks("test_click_log"));
			assertEquals(1906, dataManager.getClicks());

			dataManager.setAgeFilter(AGE_ABOVE_54,false);
			assertTrue(dataManager.fetchClicks("test_click_log"));
			assertEquals(0, dataManager.getClicks());

			dataManager.setAgeFilter(AGE_UNDER_25,true);
			assertTrue(dataManager.fetchClicks("test_click_log"));
			assertEquals(2985, dataManager.getClicks());

			dataManager.setAgeFilter(AGE_25_TO_34,true);
			assertTrue(dataManager.fetchClicks("test_click_log"));
			assertEquals(10016, dataManager.getClicks());
		}

		@Test
		@DisplayName("Checking correct results for clicks with income filter")
		void fetchClicksIncomeTest() {
			dataManager.setIncomeFilter(INCOME_LOW,false);
			assertTrue(dataManager.fetchClicks("test_click_log"));
			assertEquals(19345, dataManager.getClicks());

			dataManager.setIncomeFilter(INCOME_MEDIUM,false);
			assertTrue(dataManager.fetchClicks("test_click_log"));
			assertEquals(5556, dataManager.getClicks());

			dataManager.setIncomeFilter(INCOME_HIGH,false);
			assertTrue(dataManager.fetchClicks("test_click_log"));
			assertEquals(0, dataManager.getClicks());

			dataManager.setIncomeFilter(INCOME_LOW,true);
			assertTrue(dataManager.fetchClicks("test_click_log"));
			assertEquals(4578, dataManager.getClicks());

			dataManager.setIncomeFilter(INCOME_MEDIUM,true);
			assertTrue(dataManager.fetchClicks("test_click_log"));
			assertEquals(18367, dataManager.getClicks());

			dataManager.setIncomeFilter(INCOME_HIGH,true);
			assertTrue(dataManager.fetchClicks("test_click_log"));
			assertEquals(23923, dataManager.getClicks());
		}

		@Test
		@DisplayName("Checking correct results for clicks with context filter")
		void fetchClicksContextTest() {
			dataManager.setContextFilter(CONTEXT_BLOG, false);
			assertTrue(dataManager.fetchClicks("test_click_log"));
			assertEquals(21206, dataManager.getClicks());

			dataManager.setContextFilter(CONTEXT_NEWS, false);
			assertTrue(dataManager.fetchClicks("test_click_log"));
			assertEquals(14490, dataManager.getClicks());

			dataManager.setContextFilter(CONTEXT_SHOPPING, false);
			assertTrue(dataManager.fetchClicks("test_click_log"));
			assertEquals(7843, dataManager.getClicks());

			dataManager.setContextFilter(CONTEXT_SM, false);
			assertTrue(dataManager.fetchClicks("test_click_log"));
			assertEquals(0, dataManager.getClicks());

			dataManager.setContextFilter(CONTEXT_NEWS, true);
			assertTrue(dataManager.fetchClicks("test_click_log"));
			assertEquals(6716, dataManager.getClicks());

			dataManager.setContextFilter(CONTEXT_BLOG, true);
			assertTrue(dataManager.fetchClicks("test_click_log"));
			assertEquals(9433, dataManager.getClicks());
		}

		@Test
		@DisplayName("Checking correct results for clicks with mixed filters")
		void fetchClicksMixedTest() {
			dataManager.setGenderFilter(GENDER_FEMALE);
			dataManager.setAgeFilter(AGE_35_TO_44,false);
			dataManager.setIncomeFilter(INCOME_LOW,false);
			dataManager.setContextFilter(CONTEXT_BLOG, false);
			assertTrue(dataManager.fetchClicks("test_click_log"));
			assertEquals(8035, dataManager.getClicks());

			dataManager.setGenderFilter(GENDER_MALE);
			dataManager.setAgeFilter(AGE_ABOVE_54,false);
			dataManager.setIncomeFilter(INCOME_MEDIUM,false);
			dataManager.setContextFilter(CONTEXT_SHOPPING, false);
			assertTrue(dataManager.fetchClicks("test_click_log"));
			assertEquals(727, dataManager.getClicks());

			dataManager.setIncomeFilter(INCOME_HIGH,false);
			assertTrue(dataManager.fetchClicks("test_click_log"));
			assertEquals(0, dataManager.getClicks());
		}
	}

	@Nested
	@DisplayName("Testing uniques")
	class Uniques {
		@Test
		@DisplayName("Checking correct results for uniques")
		void fetchUniquesTest() {
			assertTrue(dataManager.fetchUniques("test_click_log"));
			assertEquals(23806, dataManager.getUniques());

			dataManager.setTimeFilterStart("2015-01-13 00:00:00");
			dataManager.setTimeFilterEnd("2015-01-13 23:59:59");
			assertTrue(dataManager.fetchUniques("test_click_log"));
			assertEquals(2053, dataManager.getUniques());

			dataManager.setTimeFilterStart("2015-01-13 16:30:00");
			dataManager.setTimeFilterEnd("2015-01-13 16:31:00");
			assertTrue(dataManager.fetchUniques("test_click_log"));
			assertEquals(2, dataManager.getUniques());
		}

		@Test
		@DisplayName("Checking correct results for uniques with gender filter")
		void fetchUniquesGenderTest() {
			dataManager.setGenderFilter(GENDER_FEMALE);
			assertTrue(dataManager.fetchUniques("test_click_log"));
			assertEquals(15856, dataManager.getUniques());

			dataManager.setGenderFilter(GENDER_MALE);
			assertTrue(dataManager.fetchUniques("test_click_log"));
			assertEquals(7950, dataManager.getUniques());
		}

		@Test
		@DisplayName("Checking correct results for uniques with age filter")
		void fetchUniquesAgeTest() {
			dataManager.setAgeFilter(AGE_UNDER_25, false);
			assertTrue(dataManager.fetchUniques("test_click_log"));
			assertEquals(20834, dataManager.getUniques());

			dataManager.setAgeFilter(AGE_25_TO_34, false);
			assertTrue(dataManager.fetchUniques("test_click_log"));
			assertEquals(13838, dataManager.getUniques());

			dataManager.setAgeFilter(AGE_35_TO_44, false);
			assertTrue(dataManager.fetchUniques("test_click_log"));
			assertEquals(6770, dataManager.getUniques());

			dataManager.setAgeFilter(AGE_45_TO_54, false);
			assertTrue(dataManager.fetchUniques("test_click_log"));
			assertEquals(1902, dataManager.getUniques());

			dataManager.setAgeFilter(AGE_ABOVE_54, false);
			assertTrue(dataManager.fetchUniques("test_click_log"));
			assertEquals(0, dataManager.getUniques());

			dataManager.setAgeFilter(AGE_UNDER_25, true);
			assertTrue(dataManager.fetchUniques("test_click_log"));
			assertEquals(2972, dataManager.getUniques());

			dataManager.setAgeFilter(AGE_35_TO_44, true);
			assertTrue(dataManager.fetchUniques("test_click_log"));
			assertEquals(10040, dataManager.getUniques());
		}

		@Test
		@DisplayName("Checking correct results for uniques with income filter")
		void fetchUniquesIncomeTest() {
			dataManager.setIncomeFilter(INCOME_LOW, false);
			assertTrue(dataManager.fetchUniques("test_click_log"));
			assertEquals(19246, dataManager.getUniques());

			dataManager.setIncomeFilter(INCOME_MEDIUM, false);
			assertTrue(dataManager.fetchUniques("test_click_log"));
			assertEquals(5528, dataManager.getUniques());

			dataManager.setIncomeFilter(INCOME_HIGH, false);
			assertTrue(dataManager.fetchUniques("test_click_log"));
			assertEquals(0, dataManager.getUniques());

			dataManager.setIncomeFilter(INCOME_LOW, true);
			assertTrue(dataManager.fetchUniques("test_click_log"));
			assertEquals(4560, dataManager.getUniques());

			dataManager.setIncomeFilter(INCOME_MEDIUM, true);
			assertTrue(dataManager.fetchUniques("test_click_log"));
			assertEquals(18278, dataManager.getUniques());

			dataManager.setIncomeFilter(INCOME_HIGH, true);
			assertTrue(dataManager.fetchUniques("test_click_log"));
			assertEquals(23806, dataManager.getUniques());
		}

		@Test
		@DisplayName("Checking correct results for uniques with context filter")
		void fetchUniquesContextTest() {
			dataManager.setContextFilter(CONTEXT_BLOG, false);
			assertTrue(dataManager.fetchUniques("test_click_log"));
			assertEquals(21107, dataManager.getUniques());

			dataManager.setContextFilter(CONTEXT_NEWS, false);
			assertTrue(dataManager.fetchUniques("test_click_log"));
			assertEquals(14425, dataManager.getUniques());

			dataManager.setContextFilter(CONTEXT_SHOPPING, false);
			assertTrue(dataManager.fetchUniques("test_click_log"));
			assertEquals(7803, dataManager.getUniques());

			dataManager.setContextFilter(CONTEXT_SM, false);
			assertTrue(dataManager.fetchUniques("test_click_log"));
			assertEquals(0, dataManager.getUniques());

			dataManager.setContextFilter(CONTEXT_NEWS, true);
			assertTrue(dataManager.fetchUniques("test_click_log"));
			assertEquals(6682, dataManager.getUniques());

			dataManager.setContextFilter(CONTEXT_BLOG, true);
			assertTrue(dataManager.fetchUniques("test_click_log"));
			assertEquals(9381, dataManager.getUniques());
		}

		@Test
		@DisplayName("Checking correct results for uniques with mixed filters")
		void fetchUniquesMixedTest() {
			dataManager.setGenderFilter(GENDER_FEMALE);
			dataManager.setAgeFilter(AGE_35_TO_44,false);
			dataManager.setIncomeFilter(INCOME_LOW,false);
			dataManager.setContextFilter(CONTEXT_BLOG, false);
			assertTrue(dataManager.fetchUniques("test_click_log"));
			assertEquals(8000, dataManager.getUniques());

			dataManager.setGenderFilter(GENDER_MALE);
			dataManager.setAgeFilter(AGE_ABOVE_54,false);
			dataManager.setIncomeFilter(INCOME_MEDIUM,false);
			dataManager.setContextFilter(CONTEXT_SHOPPING, false);
			assertTrue(dataManager.fetchUniques("test_click_log"));
			assertEquals(724, dataManager.getUniques());

			dataManager.setIncomeFilter(INCOME_HIGH,false);
			assertTrue(dataManager.fetchUniques("test_click_log"));
			assertEquals(0, dataManager.getUniques());
		}
	}

	@Nested
	@DisplayName("Testing clicks")
	class Bounces {
		@Test
		@DisplayName("Checking correct results for bounces")
		void fetchBouncesTest() {
			assertTrue(dataManager.fetchBounces("test_server_log"));
			assertEquals(6168, dataManager.getBounces());

			dataManager.setTimeFilterStart("2015-01-13 00:00:00");
			dataManager.setTimeFilterEnd("2015-01-13 23:59:59");
			assertTrue(dataManager.fetchBounces("test_server_log"));
			assertEquals(569, dataManager.getBounces());

			dataManager.setTimeFilterStart("2015-01-13 16:30:00");
			dataManager.setTimeFilterEnd("2015-01-13 16:31:00");
			assertTrue(dataManager.fetchBounces("test_server_log"));
			assertEquals(0, dataManager.getBounces());
		}

		@Test
		@DisplayName("Checking correct results for bounces with gender filter")
		void fetchBouncesGenderTest() {
			dataManager.setGenderFilter(GENDER_FEMALE);
			assertTrue(dataManager.fetchBounces("test_server_log"));
			assertEquals(4141, dataManager.getBounces());

			dataManager.setGenderFilter(GENDER_MALE);
			assertTrue(dataManager.fetchBounces("test_server_log"));
			assertEquals(2027, dataManager.getBounces());
		}

		@Test
		@DisplayName("Checking correct results for bounces with bounce redefinition")
		void fetchBouncesRedefinitionTest() {
			dataManager.setBounceFilter(BOUNCE_TIME);
			assertTrue(dataManager.fetchBounces("test_server_log"));
			assertEquals(6168, dataManager.getBounces());

			dataManager.setBounceFilter(BOUNCE_SINGLE);
			assertTrue(dataManager.fetchBounces("test_server_log"));
			assertEquals(8665, dataManager.getBounces());
		}

		@Test
		@DisplayName("Checking correct results for bounces with age filter")
		void fetchBouncesAgeTest() {
			dataManager.setAgeFilter(AGE_UNDER_25, false);
			assertTrue(dataManager.fetchBounces("test_server_log"));
			assertEquals(5430, dataManager.getBounces());

			dataManager.setAgeFilter(AGE_25_TO_34, false);
			assertTrue(dataManager.fetchBounces("test_server_log"));
			assertEquals(3592, dataManager.getBounces());

			dataManager.setAgeFilter(AGE_35_TO_44, false);
			assertTrue(dataManager.fetchBounces("test_server_log"));
			assertEquals(1756, dataManager.getBounces());

			dataManager.setAgeFilter(AGE_45_TO_54, false);
			assertTrue(dataManager.fetchBounces("test_server_log"));
			assertEquals(486, dataManager.getBounces());

			dataManager.setAgeFilter(AGE_ABOVE_54, false);
			assertTrue(dataManager.fetchBounces("test_server_log"));
			assertEquals(0, dataManager.getBounces());

			dataManager.setAgeFilter(AGE_UNDER_25, true);
			assertTrue(dataManager.fetchBounces("test_server_log"));
			assertEquals(738, dataManager.getBounces());

			dataManager.setAgeFilter(AGE_35_TO_44, true);
			assertTrue(dataManager.fetchBounces("test_server_log"));
			assertEquals(2574, dataManager.getBounces());
		}

		@Test
		@DisplayName("Checking correct results for bounces with income filter")
		void fetchBouncesIncomeTest() {
			dataManager.setIncomeFilter(INCOME_LOW, false);
			assertTrue(dataManager.fetchBounces("test_server_log"));
			assertEquals(4991, dataManager.getBounces());

			dataManager.setIncomeFilter(INCOME_MEDIUM, false);
			assertTrue(dataManager.fetchBounces("test_server_log"));
			assertEquals(1418, dataManager.getBounces());

			dataManager.setIncomeFilter(INCOME_HIGH, false);
			assertTrue(dataManager.fetchBounces("test_server_log"));
			assertEquals(0, dataManager.getBounces());

			dataManager.setIncomeFilter(INCOME_LOW, true);
			assertTrue(dataManager.fetchBounces("test_server_log"));
			assertEquals(1177, dataManager.getBounces());

			dataManager.setIncomeFilter(INCOME_MEDIUM, true);
			assertTrue(dataManager.fetchBounces("test_server_log"));
			assertEquals(4750, dataManager.getBounces());

			dataManager.setIncomeFilter(INCOME_HIGH, true);
			assertTrue(dataManager.fetchBounces("test_server_log"));
			assertEquals(6168, dataManager.getBounces());
		}

		@Test
		@DisplayName("Checking correct results for bounces with context filter")
		void fetchBouncesContextTest() {
			dataManager.setContextFilter(CONTEXT_BLOG, false);
			assertTrue(dataManager.fetchBounces("test_server_log"));
			assertEquals(5493, dataManager.getBounces());

			dataManager.setContextFilter(CONTEXT_NEWS, false);
			assertTrue(dataManager.fetchBounces("test_server_log"));
			assertEquals(3783, dataManager.getBounces());

			dataManager.setContextFilter(CONTEXT_SHOPPING, false);
			assertTrue(dataManager.fetchBounces("test_server_log"));
			assertEquals(2036, dataManager.getBounces());

			dataManager.setContextFilter(CONTEXT_SM, false);
			assertTrue(dataManager.fetchBounces("test_server_log"));
			assertEquals(0, dataManager.getBounces());

			dataManager.setContextFilter(CONTEXT_NEWS, true);
			assertTrue(dataManager.fetchBounces("test_server_log"));
			assertEquals(1710, dataManager.getBounces());

			dataManager.setContextFilter(CONTEXT_BLOG, true);
			assertTrue(dataManager.fetchBounces("test_server_log"));
			assertEquals(2385, dataManager.getBounces());
		}

		@Test
		@DisplayName("Checking correct results for bounces with mixed filters")
		void fetchBouncesMixedTest() {
			dataManager.setBounceFilter(BOUNCE_SINGLE);
			dataManager.setGenderFilter(GENDER_FEMALE);
			dataManager.setAgeFilter(AGE_35_TO_44,false);
			dataManager.setIncomeFilter(INCOME_LOW,false);
			dataManager.setContextFilter(CONTEXT_BLOG, false);
			assertTrue(dataManager.fetchBounces("test_server_log"));
			assertEquals(2933, dataManager.getBounces());

			dataManager.setGenderFilter(GENDER_MALE);
			dataManager.setAgeFilter(AGE_ABOVE_54,false);
			dataManager.setIncomeFilter(INCOME_MEDIUM,false);
			dataManager.setContextFilter(CONTEXT_SHOPPING, false);
			assertTrue(dataManager.fetchBounces("test_server_log"));
			assertEquals(263, dataManager.getBounces());

			dataManager.setBounceFilter(BOUNCE_TIME);
			dataManager.setIncomeFilter(INCOME_HIGH,false);
			assertTrue(dataManager.fetchBounces("test_server_log"));
			assertEquals(0, dataManager.getBounces());
		}
	}

	@Nested
	@DisplayName("Testing conversions")
	class Conversions {
		@Test
		@DisplayName("Checking correct results for conversions")
		void fetchConverionsTest() {
			assertTrue(dataManager.fetchConversions("test_server_log"));
			assertEquals(2026, dataManager.getConversions());

			dataManager.setTimeFilterStart("2015-01-13 00:00:00");
			dataManager.setTimeFilterEnd("2015-01-13 23:59:59");
			assertTrue(dataManager.fetchConversions("test_server_log"));
			assertEquals(141, dataManager.getConversions());

			dataManager.setTimeFilterStart("2015-01-13 16:30:00");
			dataManager.setTimeFilterEnd("2015-01-13 16:31:00");
			assertTrue(dataManager.fetchConversions("test_server_log"));
			assertEquals(0, dataManager.getConversions());
		}

		@Test
		@DisplayName("Checking correct results for conversions with gender filter")
		void fetchConverionsGenderTest() {
			dataManager.setGenderFilter(GENDER_FEMALE);
			assertTrue(dataManager.fetchConversions("test_server_log"));
			assertEquals(1352, dataManager.getConversions());

			dataManager.setGenderFilter(GENDER_MALE);
			assertTrue(dataManager.fetchConversions("test_server_log"));
			assertEquals(674, dataManager.getConversions());
		}

		@Test
		@DisplayName("Checking correct results for conversions with age filter")
		void fetchConverionsAgeTest() {
			dataManager.setAgeFilter(AGE_UNDER_25, false);
			assertTrue(dataManager.fetchConversions("test_server_log"));
			assertEquals(1842, dataManager.getConversions());

			dataManager.setAgeFilter(AGE_25_TO_34, false);
			assertTrue(dataManager.fetchConversions("test_server_log"));
			assertEquals(1225, dataManager.getConversions());

			dataManager.setAgeFilter(AGE_35_TO_44, false);
			assertTrue(dataManager.fetchConversions("test_server_log"));
			assertEquals(548, dataManager.getConversions());

			dataManager.setAgeFilter(AGE_45_TO_54, false);
			assertTrue(dataManager.fetchConversions("test_server_log"));
			assertEquals(94, dataManager.getConversions());

			dataManager.setAgeFilter(AGE_ABOVE_54, false);
			assertTrue(dataManager.fetchConversions("test_server_log"));
			assertEquals(0, dataManager.getConversions());

			dataManager.setAgeFilter(AGE_UNDER_25, true);
			assertTrue(dataManager.fetchConversions("test_server_log"));
			assertEquals(184, dataManager.getConversions());

			dataManager.setAgeFilter(AGE_35_TO_44, true);
			assertTrue(dataManager.fetchConversions("test_server_log"));
			assertEquals(861, dataManager.getConversions());
		}

		@Test
		@DisplayName("Checking correct results for conversions with income filter")
		void fetchConverionsIncomeTest() {
			dataManager.setIncomeFilter(INCOME_LOW, false);
			assertTrue(dataManager.fetchConversions("test_server_log"));
			assertEquals(1766, dataManager.getConversions());

			dataManager.setIncomeFilter(INCOME_MEDIUM, false);
			assertTrue(dataManager.fetchConversions("test_server_log"));
			assertEquals(522, dataManager.getConversions());

			dataManager.setIncomeFilter(INCOME_HIGH, false);
			assertTrue(dataManager.fetchConversions("test_server_log"));
			assertEquals(0, dataManager.getConversions());

			dataManager.setIncomeFilter(INCOME_LOW, true);
			assertTrue(dataManager.fetchConversions("test_server_log"));
			assertEquals(260, dataManager.getConversions());

			dataManager.setIncomeFilter(INCOME_MEDIUM, true);
			assertTrue(dataManager.fetchConversions("test_server_log"));
			assertEquals(1504, dataManager.getConversions());

			dataManager.setIncomeFilter(INCOME_HIGH, true);
			assertTrue(dataManager.fetchConversions("test_server_log"));
			assertEquals(2026, dataManager.getConversions());
		}

		@Test
		@DisplayName("Checking correct results for conversions with context filter")
		void fetchConverionsContextTest() {
			dataManager.setContextFilter(CONTEXT_BLOG, false);
			assertTrue(dataManager.fetchConversions("test_server_log"));
			assertEquals(1827, dataManager.getConversions());

			dataManager.setContextFilter(CONTEXT_NEWS, false);
			assertTrue(dataManager.fetchConversions("test_server_log"));
			assertEquals(1264, dataManager.getConversions());

			dataManager.setContextFilter(CONTEXT_SHOPPING, false);
			assertTrue(dataManager.fetchConversions("test_server_log"));
			assertEquals(695, dataManager.getConversions());

			dataManager.setContextFilter(CONTEXT_SM, false);
			assertTrue(dataManager.fetchConversions("test_server_log"));
			assertEquals(0, dataManager.getConversions());

			dataManager.setContextFilter(CONTEXT_NEWS, true);
			assertTrue(dataManager.fetchConversions("test_server_log"));
			assertEquals(563, dataManager.getConversions());

			dataManager.setContextFilter(CONTEXT_BLOG, true);
			assertTrue(dataManager.fetchConversions("test_server_log"));
			assertEquals(762, dataManager.getConversions());
		}

		@Test
		@DisplayName("Checking correct results for conversions with mixed filters")
		void fetchConverionsMixedTest() {
			dataManager.setGenderFilter(GENDER_FEMALE);
			dataManager.setAgeFilter(AGE_35_TO_44,false);
			dataManager.setIncomeFilter(INCOME_LOW,false);
			dataManager.setContextFilter(CONTEXT_BLOG, false);
			assertTrue(dataManager.fetchConversions("test_server_log"));
			assertEquals(698, dataManager.getConversions());

			dataManager.setGenderFilter(GENDER_MALE);
			dataManager.setAgeFilter(AGE_ABOVE_54,false);
			dataManager.setIncomeFilter(INCOME_MEDIUM,false);
			dataManager.setContextFilter(CONTEXT_SHOPPING, false);
			assertTrue(dataManager.fetchConversions("test_server_log"));
			assertEquals(74, dataManager.getConversions());

			dataManager.setIncomeFilter(INCOME_HIGH,false);
			assertTrue(dataManager.fetchConversions("test_server_log"));
			assertEquals(0, dataManager.getConversions());
		}
	}

	@Nested
	@DisplayName("Testing cost")
	class Cost {
		@Test
		@DisplayName("Checking correct results for total cost")
		void fetchCostTest() {
			assertTrue(dataManager.fetchCost("test_click_log", "test_impression_log"));
			assertEquals(118097.92, dataManager.getCost());

			dataManager.setTimeFilterStart("2015-01-13 00:00:00");
			dataManager.setTimeFilterEnd("2015-01-13 23:59:59");
			assertTrue(dataManager.fetchCost("test_click_log", "test_impression_log"));
			assertEquals(10175.39, dataManager.getCost());

			dataManager.setTimeFilterStart("2015-01-13 16:30:00");
			dataManager.setTimeFilterEnd("2015-01-13 16:31:00");
			assertTrue(dataManager.fetchCost("test_click_log", "test_impression_log"));
			assertEquals(6.99, dataManager.getCost());
		}

		@Test
		@DisplayName("Checking correct results for total cost with gender filter")
		void fetchCostGenderTest() {
			dataManager.setGenderFilter(GENDER_FEMALE);
			assertTrue(dataManager.fetchCost("test_click_log", "test_impression_log"));
			assertEquals(78768.33, dataManager.getCost());

			dataManager.setGenderFilter(GENDER_MALE);
			assertTrue(dataManager.fetchCost("test_click_log", "test_impression_log"));
			assertEquals(39329.58, dataManager.getCost());
		}

		@Test
		@DisplayName("Checking correct results for total cost with age filter")
		void fetchCostAgeTest() {
			dataManager.setAgeFilter(AGE_UNDER_25, false);
			assertTrue(dataManager.fetchCost("test_click_log", "test_impression_log"));
			assertEquals(103695.04, dataManager.getCost());

			dataManager.setAgeFilter(AGE_25_TO_34, false);
			assertTrue(dataManager.fetchCost("test_click_log", "test_impression_log"));
			assertEquals(68680.53, dataManager.getCost());

			dataManager.setAgeFilter(AGE_35_TO_44, false);
			assertTrue(dataManager.fetchCost("test_click_log", "test_impression_log"));
			assertEquals(33329.74, dataManager.getCost());

			dataManager.setAgeFilter(AGE_45_TO_54, false);
			assertTrue(dataManager.fetchCost("test_click_log", "test_impression_log"));
			assertEquals(9646.23, dataManager.getCost());

			dataManager.setAgeFilter(AGE_ABOVE_54, false);
			assertTrue(dataManager.fetchCost("test_click_log", "test_impression_log"));
			assertEquals(0.0, dataManager.getCost());

			dataManager.setAgeFilter(AGE_UNDER_25, true);
			assertTrue(dataManager.fetchCost("test_click_log", "test_impression_log"));
			assertEquals(14402.87, dataManager.getCost());

			dataManager.setAgeFilter(AGE_35_TO_44, true);
			assertTrue(dataManager.fetchCost("test_click_log", "test_impression_log"));
			assertEquals(49753.65, dataManager.getCost());
		}

		@Test
		@DisplayName("Checking correct results for total cost with income filter")
		void fetchCostIncomeTest() {
			dataManager.setIncomeFilter(INCOME_LOW, false);
			assertTrue(dataManager.fetchCost("test_click_log", "test_impression_log"));
			assertEquals(95992.36, dataManager.getCost());

			dataManager.setIncomeFilter(INCOME_MEDIUM, false);
			assertTrue(dataManager.fetchCost("test_click_log", "test_impression_log"));
			assertEquals(27433.88, dataManager.getCost());

			dataManager.setIncomeFilter(INCOME_HIGH, false);
			assertTrue(dataManager.fetchCost("test_click_log", "test_impression_log"));
			assertEquals(0, dataManager.getCost());

			dataManager.setIncomeFilter(INCOME_LOW, true);
			assertTrue(dataManager.fetchCost("test_click_log", "test_impression_log"));
			assertEquals(22105.56, dataManager.getCost());

			dataManager.setIncomeFilter(INCOME_MEDIUM, true);
			assertTrue(dataManager.fetchCost("test_click_log", "test_impression_log"));
			assertEquals(90664.03, dataManager.getCost());

			dataManager.setIncomeFilter(INCOME_HIGH, true);
			assertTrue(dataManager.fetchCost("test_click_log", "test_impression_log"));
			assertEquals(118097.92, dataManager.getCost());
		}

		@Test
		@DisplayName("Checking correct results for total cost with context filter")
		void fetchCostContextTest() {
			dataManager.setContextFilter(CONTEXT_BLOG, false);
			assertTrue(dataManager.fetchCost("test_click_log", "test_impression_log"));
			assertEquals(104495.85, dataManager.getCost());

			dataManager.setContextFilter(CONTEXT_NEWS, false);
			assertTrue(dataManager.fetchCost("test_click_log", "test_impression_log"));
			assertEquals(72138.05, dataManager.getCost());

			dataManager.setContextFilter(CONTEXT_SHOPPING, false);
			assertTrue(dataManager.fetchCost("test_click_log", "test_impression_log"));
			assertEquals(39640.4, dataManager.getCost());

			dataManager.setContextFilter(CONTEXT_SM, false);
			assertTrue(dataManager.fetchCost("test_click_log", "test_impression_log"));
			assertEquals(0, dataManager.getCost());

			dataManager.setContextFilter(CONTEXT_NEWS, true);
			assertTrue(dataManager.fetchCost("test_click_log", "test_impression_log"));
			assertEquals(32357.8, dataManager.getCost());

			dataManager.setContextFilter(CONTEXT_BLOG, true);
			assertTrue(dataManager.fetchCost("test_click_log", "test_impression_log"));
			assertEquals(45959.87, dataManager.getCost());
		}

		@Test
		@DisplayName("Checking correct results for total cost with mixed filters")
		void fetchCostMixedTest() {
			dataManager.setGenderFilter(GENDER_FEMALE);
			dataManager.setAgeFilter(AGE_35_TO_44,false);
			dataManager.setIncomeFilter(INCOME_LOW,false);
			dataManager.setContextFilter(CONTEXT_BLOG, false);
			assertTrue(dataManager.fetchCost("test_click_log", "test_impression_log"));
			assertEquals(39208.21, dataManager.getCost());

			dataManager.setGenderFilter(GENDER_MALE);
			dataManager.setAgeFilter(AGE_ABOVE_54,false);
			dataManager.setIncomeFilter(INCOME_MEDIUM,false);
			dataManager.setContextFilter(CONTEXT_SHOPPING, false);
			assertTrue(dataManager.fetchCost("test_click_log", "test_impression_log"));
			assertEquals(3725.06, dataManager.getCost());

			dataManager.setIncomeFilter(INCOME_HIGH,false);
			assertTrue(dataManager.fetchCost("test_click_log", "test_impression_log"));
			assertEquals(0.0, dataManager.getCost());
		}
	}

	@AfterAll
	static void afterAll() throws SQLException {
		reader.dropTable("test_impression_log");
		reader.dropTable("test_click_log");
		reader.dropTable("test_server_log");
	}

	@BeforeEach
	void beforeEach() {
		dataManager.resetFilters();
	}
}