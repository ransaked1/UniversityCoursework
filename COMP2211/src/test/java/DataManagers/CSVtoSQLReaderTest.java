package DataManagers;

import DataManagers.CSVtoSQLReader;
import org.junit.jupiter.api.*;

import java.io.File;
import java.sql.SQLException;
import java.util.ArrayList;

import static org.junit.jupiter.api.Assertions.*;

class CSVtoSQLReaderTest {

	static CSVtoSQLReader reader = new CSVtoSQLReader("input_db_test");

	@BeforeEach
	void setUp() throws SQLException {
		reader.dropTable("impression_log");
		reader.dropTable("server_log");
		reader.dropTable("click_log");
		reader.dropTable("test");
	}

	@AfterEach
	void tearDown() throws SQLException {
		reader.dropTable("impression_log");
		reader.dropTable("server_log");
		reader.dropTable("click_log");
		reader.dropTable("test");
	}

	@Test
	@DisplayName("Table existance checker test")
	void checkTableExistsTest() throws SQLException {
		reader.setupInputTable("test", "test1,test2,test3");
		assertTrue(reader.checkTableExists("test"));
		assertFalse(reader.checkTableExists(""));
		assertFalse(reader.checkTableExists("invalid_table"));
	}

	@Test
	@DisplayName("Drop table test")
	void dropTableTest() throws SQLException {
		reader.setupInputTable("test", "test1,test2,test3");
		reader.dropTable("test");
		assertFalse(reader.checkTableExists("test"));
		reader.dropTable("test");
	}

	@Nested
	@DisplayName("Testing CSV data reader")
	class Reader {
		@Test
		@DisplayName("Writing CSV data to table: valid")
		void writeLogToTableValidTest() throws SQLException {
			reader.setupInputTable("test", "test1,test2,test3");
			assertTrue(reader.writeLogToTable("src/test/resources/valid.csv", "test"));
			ArrayList<Object> result = reader.getColumnContents("test", "test1");
			ArrayList<String> expected = new ArrayList<>();
			expected.add("fdag");
			expected.add("hrfgh");
			expected.add("htgrh");
			assertTrue(result.equals(expected));
		}

		@Test
		@DisplayName("Writing CSV data to table: extra column")
		void writeLogToTableExtraTest() throws SQLException {
			reader.setupInputTable("test", "test1,test2,test3");
			assertFalse(reader.writeLogToTable("src/test/resources/extra_column.csv", "test"));
		}

		@Test
		@DisplayName("Writing CSV data to table: missing values")
		void writeLogToTableMissingValuesTest() throws SQLException {
			reader.setupInputTable("test", "test1,test2,test3");
			assertTrue(reader.writeLogToTable("src/test/resources/missing_values.csv", "test"));
		}

		@Test
		@DisplayName("Writing CSV data to table: no values")
		void writeLogToTableNoValuesTest() throws SQLException {
			reader.setupInputTable("test", "test1,test2,test3");
			assertTrue(reader.writeLogToTable("src/test/resources/no_values.csv", "test"));
			ArrayList<Object> result = reader.getColumnContents("test", "test1");
			ArrayList<String> expected = new ArrayList<>();
			//System.out.println(result.toString());
			assertTrue(result.equals(expected));
		}

		@Test
		@DisplayName("Writing CSV data to table: type missmatch")
		void writeLogToTableMissmatchTest() throws SQLException {
			reader.setupInputTable("test", "test1,test2,test3");
			assertFalse(reader.writeLogToTable("src/test/resources/missmatch.csv", "test"));
		}

		@Test
		@DisplayName("Writing CSV data to table: non_existent file")
		void writeLogToTableInvalidTest() {
			assertFalse(reader.writeLogToTable("input/invalid.csv", "log"));
		}
	}

	@Nested
	@DisplayName("Testing input table setup")
	class setupTable {
		@Test
		@DisplayName("Writing CSV data to table: base case")
		void setupInputTableTest() throws SQLException {
			assertTrue(reader.setupInputTable("test", "test1,test2,test3"));
		}

		@Test
		@DisplayName("Writing CSV data to table: non-string header")
		void setupInputTableHeaderTest() throws SQLException {
			assertFalse(reader.setupInputTable("test", "test1,123,test3"));
		}

		@Test
		@DisplayName("Writing CSV data to table: empty")
		void setupInputTableEmptyTest() throws SQLException {
			assertFalse(reader.setupInputTable("test", ""));
		}
	}

	@AfterAll
	static void afterAll() throws SQLException {
		reader.dropTable("impression_log");
		reader.dropTable("click_log");
		reader.dropTable("server_log");
		reader.dropTable("test");
	}
}